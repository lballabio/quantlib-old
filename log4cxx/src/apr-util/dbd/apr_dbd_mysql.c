/* Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "apu.h"
#define HAVE_MYSQL_MYSQL_H

#if APU_HAVE_MYSQL

#include "apu_version.h"
#include "apu_config.h"

#include <ctype.h>
#include <stdlib.h>

#ifdef HAVE_MYSQL_H
#include <mysql.h>
#include <errmsg.h>
#elif defined(HAVE_MYSQL_MYSQL_H)
#include <mysql/mysql.h>
#include <mysql/errmsg.h>
#endif

#include "apr_strings.h"
#include "apr_buckets.h"

#include "apr_dbd_internal.h"

/* default maximum field size 1 MB */
#define FIELDSIZE 1048575

struct apr_dbd_prepared_t {
    MYSQL_STMT* stmt;
};

struct apr_dbd_transaction_t {
    int errnum;
    apr_dbd_t *handle;
};

struct apr_dbd_t {
    MYSQL* conn ;
    apr_dbd_transaction_t* trans ;
    unsigned long fldsz;
};

struct apr_dbd_results_t {
    int random;
    MYSQL_RES *res;
    MYSQL_STMT *statement;
    MYSQL_BIND *bind;
};
struct apr_dbd_row_t {
    MYSQL_ROW row;
    apr_dbd_results_t *res;
};

static apr_status_t free_result(void *data)
{
    mysql_free_result(data);
    return APR_SUCCESS;
}

static int dbd_mysql_select(apr_pool_t *pool, apr_dbd_t *sql,
                            apr_dbd_results_t **results,
                            const char *query, int seek)
{
    int sz;
    int ret;
    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }
    ret = mysql_query(sql->conn, query);
    if (!ret) {
        if (sz = mysql_field_count(sql->conn), sz > 0) {
            if (!*results) {
                *results = apr_palloc(pool, sizeof(apr_dbd_results_t));
            }
            (*results)->random = seek;
            (*results)->statement = NULL;
            if (seek) {
                (*results)->res = mysql_store_result(sql->conn);
            }
            else {
                (*results)->res = mysql_use_result(sql->conn);
            }
            apr_pool_cleanup_register(pool, (*results)->res,
                                      free_result,apr_pool_cleanup_null);
        }
    } else {
        ret = mysql_errno(sql->conn);
    }
    
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static int dbd_mysql_get_row(apr_pool_t *pool, apr_dbd_results_t *res,
                             apr_dbd_row_t **row, int rownum)
{
    MYSQL_ROW r = NULL;
    int ret = 0;

    if (res->statement) {
        if (res->random) {
            if (rownum >= 0) {
                mysql_stmt_data_seek(res->statement, (my_ulonglong)rownum);
            }
        }
        ret = mysql_stmt_fetch(res->statement);
        switch (ret) {
        case 1:
            ret = mysql_stmt_errno(res->statement);
            break;
        case MYSQL_NO_DATA:
            ret = -1;
            break;
        default:
            ret = 0; /* bad luck - get_entry will deal with this */
            break;
        }
    }
    else {
        if (res->random) {
            if (rownum >= 0) {
                mysql_data_seek(res->res, (my_ulonglong) rownum);
            }
        }
        r = mysql_fetch_row(res->res);
        if (r == NULL) {
            ret = -1;
        }
    }
    if (ret == 0) {
        if (!*row) {
            *row = apr_palloc(pool, sizeof(apr_dbd_row_t));
        }
        (*row)->row = r;
        (*row)->res = res;
    }
    else {
        apr_pool_cleanup_run(pool, res->res, free_result);
    }
    return ret;
}
#if 0
/* An improved API that was proposed but not followed up */
static int dbd_mysql_get_entry(const apr_dbd_row_t *row, int n,
                               apr_dbd_datum_t *val)
{
    MYSQL_BIND *bind;
    if (row->res->statement) {
        bind = &row->res->bind[n];
        if (mysql_stmt_fetch_column(row->res->statement, bind, n, 0) != 0) {
            val->type = APR_DBD_VALUE_NULL;
            return -1;
        }
        if (*bind->is_null) {
            val->type = APR_DBD_VALUE_NULL;
            return -1;
        }
        else {
            val->type = APR_DBD_VALUE_STRING;
            val->value.stringval = bind->buffer;
        }
    }
    else {
        val->type = APR_DBD_VALUE_STRING;
        val->value.stringval = row->row[n];
    }
    return 0;
}
#else

static const char *dbd_mysql_get_entry(const apr_dbd_row_t *row, int n)
{
    MYSQL_BIND *bind;
    if (row->res->statement) {
        bind = &row->res->bind[n];
        if (mysql_stmt_fetch_column(row->res->statement, bind, n, 0) != 0) {
            return NULL;
        }
        if (*bind->is_null) {
            return NULL;
        }
        else {
            return bind->buffer;
        }
    }
    else {
        return row->row[n];
    }
    return NULL;
}
#endif

static const char *dbd_mysql_error(apr_dbd_t *sql, int n)
{
    return mysql_error(sql->conn);
}

static int dbd_mysql_query(apr_dbd_t *sql, int *nrows, const char *query)
{
    int ret;
    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }
    ret = mysql_query(sql->conn, query);
    if (ret != 0) {
        ret = mysql_errno(sql->conn);
    }
    *nrows = mysql_affected_rows(sql->conn);
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static const char *dbd_mysql_escape(apr_pool_t *pool, const char *arg,
                                    apr_dbd_t *sql)
{
    unsigned long len = strlen(arg);
    char *ret = apr_palloc(pool, 2*len + 1);
    mysql_real_escape_string(sql->conn, ret, arg, len);
    return ret;
}

static apr_status_t stmt_close(void *data)
{
    mysql_stmt_close(data);
    return APR_SUCCESS;
}

static int dbd_mysql_prepare(apr_pool_t *pool, apr_dbd_t *sql,
                             const char *query, const char *label,
                             apr_dbd_prepared_t **statement)
{
    /* Translate from apr_dbd to native query format */
    char *myquery = apr_pstrdup(pool, query);
    char *p = myquery;
    const char *q;
    int ret;
    for (q = query; *q; ++q) {
        if (q[0] == '%') {
            if (isalpha(q[1])) {
                *p++ = '?';
                ++q;
            }
            else if (q[1] == '%') {
                /* reduce %% to % */
                *p++ = *q++;
            }
            else {
                *p++ = *q;
            }
        }
        else {
            *p++ = *q;
        }
    } 
    *p = 0;
    if (!*statement) {
        *statement = apr_palloc(pool, sizeof(apr_dbd_prepared_t));
    }
    (*statement)->stmt = mysql_stmt_init(sql->conn);

    if ((*statement)->stmt) {
        apr_pool_cleanup_register(pool, (*statement)->stmt,
                                  stmt_close, apr_pool_cleanup_null);
        ret = mysql_stmt_prepare((*statement)->stmt, myquery, strlen(myquery));

        if (ret != 0) {
            ret = mysql_stmt_errno((*statement)->stmt);
        }

        return ret;
    }

    return CR_OUT_OF_MEMORY;
}

static int dbd_mysql_pquery(apr_pool_t *pool, apr_dbd_t *sql,
                            int *nrows, apr_dbd_prepared_t *statement,
                            int nargs, const char **values)
{
    MYSQL_BIND *bind;
    char *arg;
    int ret;
    int i;
    my_bool is_null = FALSE;

    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }
    nargs = mysql_stmt_param_count(statement->stmt);

    bind = apr_palloc(pool, nargs*sizeof(MYSQL_BIND));
    for (i=0; i < nargs; ++i) {
        arg = (char*)values[i];
        bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
        bind[i].buffer = arg;
        bind[i].buffer_length = strlen(arg);
        bind[i].length = &bind[i].buffer_length;
        bind[i].is_null = &is_null;
        bind[i].is_unsigned = 0;
    }

    ret = mysql_stmt_bind_param(statement->stmt, bind);
    if (ret != 0) {
        *nrows = 0;
        ret = mysql_stmt_errno(statement->stmt);
    }
    else {
        ret = mysql_stmt_execute(statement->stmt);
        if (ret != 0) {
            ret = mysql_stmt_errno(statement->stmt);
        }
        *nrows = mysql_stmt_affected_rows(statement->stmt);
    }
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static int dbd_mysql_pvquery(apr_pool_t *pool, apr_dbd_t *sql, int *nrows,
                             apr_dbd_prepared_t *statement, va_list args)
{
    MYSQL_BIND *bind;
    char *arg;
    int ret;
    int nargs = 0;
    int i;
    my_bool is_null = FALSE;

    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }
    nargs = mysql_stmt_param_count(statement->stmt);

    bind = apr_palloc(pool, nargs*sizeof(MYSQL_BIND));
    for (i=0; i < nargs; ++i) {
        arg = va_arg(args, char*);
        bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
        bind[i].buffer = arg;
        bind[i].buffer_length = strlen(arg);
        bind[i].length = &bind[i].buffer_length;
        bind[i].is_null = &is_null;
        bind[i].is_unsigned = 0;
    }

    ret = mysql_stmt_bind_param(statement->stmt, bind);
    if (ret != 0) {
        *nrows = 0;
        ret = mysql_stmt_errno(statement->stmt);
    }
    else {
        ret = mysql_stmt_execute(statement->stmt);
        if (ret != 0) {
            ret = mysql_stmt_errno(statement->stmt);
        }
        *nrows = mysql_stmt_affected_rows(statement->stmt);
    }
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static int dbd_mysql_pselect(apr_pool_t *pool, apr_dbd_t *sql,
                             apr_dbd_results_t **res,
                             apr_dbd_prepared_t *statement, int random,
                             int nargs, const char **args)
{
    int i;
    int nfields;
    char *arg;
    my_bool is_null = FALSE;
    my_bool *is_nullr;
#if MYSQL_VERSION_ID >= 50000
    my_bool *error;
#endif
    int ret;
    unsigned long *length, maxlen;
    MYSQL_BIND *bind;

    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }

    nargs = mysql_stmt_param_count(statement->stmt);
    bind = apr_palloc(pool, nargs*sizeof(MYSQL_BIND));

    for (i=0; i < nargs; ++i) {
        arg = (char*)args[i];
        bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
        bind[i].buffer = arg;
        bind[i].buffer_length = strlen(arg);
        bind[i].length = &bind[i].buffer_length;
        bind[i].is_null = &is_null;
        bind[i].is_unsigned = 0;
    }

    ret = mysql_stmt_bind_param(statement->stmt, bind);
    if (ret == 0) {
        ret = mysql_stmt_execute(statement->stmt);
        if (!ret) {
            if (!*res) {
                *res = apr_pcalloc(pool, sizeof(apr_dbd_results_t));
            }
            (*res)->random = random;
            (*res)->statement = statement->stmt;
            (*res)->res = mysql_stmt_result_metadata(statement->stmt);
            apr_pool_cleanup_register(pool, (*res)->res,
                                      free_result, apr_pool_cleanup_null);
            nfields = mysql_num_fields((*res)->res);
            if (!(*res)->bind) {
                (*res)->bind = apr_palloc(pool, nfields*sizeof(MYSQL_BIND));
                length = apr_pcalloc(pool, nfields*sizeof(unsigned long));
#if MYSQL_VERSION_ID >= 50000
                error = apr_palloc(pool, nfields*sizeof(my_bool));
#endif
                is_nullr = apr_pcalloc(pool, nfields*sizeof(my_bool));
                for ( i = 0; i < nfields; ++i ) {
                    maxlen = ((*res)->res->fields[i].length < sql->fldsz ?
                              (*res)->res->fields[i].length : sql->fldsz) + 1;
                    (*res)->bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
                    (*res)->bind[i].buffer_length = maxlen;
                    (*res)->bind[i].length = &length[i];
                    (*res)->bind[i].buffer = apr_palloc(pool, maxlen);
                    (*res)->bind[i].is_null = is_nullr+i;
#if MYSQL_VERSION_ID >= 50000
                    (*res)->bind[i].error = error+i;
#endif
                }
            }
            ret = mysql_stmt_bind_result(statement->stmt, (*res)->bind);
            if (!ret) {
                ret = mysql_stmt_store_result(statement->stmt);
            }
        }
    }
    if (ret != 0) {
        ret = mysql_stmt_errno(statement->stmt);
    }
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static int dbd_mysql_pvselect(apr_pool_t *pool, apr_dbd_t *sql,
                              apr_dbd_results_t **res,
                              apr_dbd_prepared_t *statement, int random,
                              va_list args)
{
    int i;
    int nfields;
    char *arg;
    my_bool is_null = FALSE;
    my_bool *is_nullr;
#if MYSQL_VERSION_ID >= 50000
    my_bool *error;
#endif
    int ret;
    unsigned long *length, maxlen;
    int nargs;
    MYSQL_BIND *bind;

    if (sql->trans && sql->trans->errnum) {
        return sql->trans->errnum;
    }

    nargs = mysql_stmt_param_count(statement->stmt);
    bind = apr_palloc(pool, nargs*sizeof(MYSQL_BIND));

    for (i=0; i < nargs; ++i) {
        arg = va_arg(args, char*);
        bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
        bind[i].buffer = arg;
        bind[i].buffer_length = strlen(arg);
        bind[i].length = &bind[i].buffer_length;
        bind[i].is_null = &is_null;
        bind[i].is_unsigned = 0;
    }

    ret = mysql_stmt_bind_param(statement->stmt, bind);
    if (ret == 0) {
        ret = mysql_stmt_execute(statement->stmt);
        if (!ret) {
            if (!*res) {
                *res = apr_pcalloc(pool, sizeof(apr_dbd_results_t));
            }
            (*res)->random = random;
            (*res)->statement = statement->stmt;
            (*res)->res = mysql_stmt_result_metadata(statement->stmt);
            apr_pool_cleanup_register(pool, (*res)->res,
                                      free_result, apr_pool_cleanup_null);
            nfields = mysql_num_fields((*res)->res);
            if (!(*res)->bind) {
                (*res)->bind = apr_palloc(pool, nfields*sizeof(MYSQL_BIND));
                length = apr_pcalloc(pool, nfields*sizeof(unsigned long));
#if MYSQL_VERSION_ID >= 50000
                error = apr_palloc(pool, nfields*sizeof(my_bool));
#endif
                is_nullr = apr_pcalloc(pool, nfields*sizeof(my_bool));
                for ( i = 0; i < nfields; ++i ) {
                    maxlen = ((*res)->res->fields[i].length < sql->fldsz ?
                              (*res)->res->fields[i].length : sql->fldsz) + 1;
                    (*res)->bind[i].buffer_type = MYSQL_TYPE_VAR_STRING;
                    (*res)->bind[i].buffer_length = maxlen;
                    (*res)->bind[i].length = &length[i];
                    (*res)->bind[i].buffer = apr_palloc(pool, maxlen);
                    (*res)->bind[i].is_null = is_nullr+i;
#if MYSQL_VERSION_ID >= 50000
                    (*res)->bind[i].error = error+i;
#endif
                }
            }
            ret = mysql_stmt_bind_result(statement->stmt, (*res)->bind);
            if (!ret) {
                ret = mysql_stmt_store_result(statement->stmt);
            }
        }
    }
    if (ret != 0) {
        ret = mysql_stmt_errno(statement->stmt);
    }
    if (sql->trans) {
        sql->trans->errnum = ret;
    }
    return ret;
}

static int dbd_mysql_end_transaction(apr_dbd_transaction_t *trans)
{
    int ret = -1;
    if (trans) {
        if (trans->errnum) {
            trans->errnum = 0;
            ret = mysql_rollback(trans->handle->conn);
        }
        else {
            ret = mysql_commit(trans->handle->conn);
        }
    }
    ret |= mysql_autocommit(trans->handle->conn, 1);
    trans->handle->trans = NULL;
    return ret;
}
/* Whether or not transactions work depends on whether the
 * underlying DB supports them within MySQL.  Unfortunately
 * it fails silently with the default InnoDB.
 */

static int dbd_mysql_transaction(apr_pool_t *pool, apr_dbd_t *handle,
                                 apr_dbd_transaction_t **trans)
{
    /* Don't try recursive transactions here */
    if (handle->trans) {
        dbd_mysql_end_transaction(handle->trans) ;
    }
    if (!*trans) {
        *trans = apr_pcalloc(pool, sizeof(apr_dbd_transaction_t));
    }
    (*trans)->errnum = mysql_autocommit(handle->conn, 0);
    (*trans)->handle = handle;
    handle->trans = *trans;
    return (*trans)->errnum;
}

static apr_dbd_t *dbd_mysql_open(apr_pool_t *pool, const char *params)
{
    static const char *const delims = " \r\n\t;|,";
    const char *ptr;
    int i;
    const char *key;
    size_t klen;
    const char *value;
    size_t vlen;
#if MYSQL_VERSION_ID >= 50013
    my_bool do_reconnect = 1;
#endif
    MYSQL *real_conn;
    unsigned long flags = 0;
    
    struct {
        const char *field;
        const char *value;
    } fields[] = {
        {"host", NULL},
        {"user", NULL},
        {"pass", NULL},
        {"dbname", NULL},
        {"port", NULL},
        {"sock", NULL},
        {"flags", NULL},
        {"fldsz", NULL},
        {"group", NULL},
        {NULL, NULL}
    };
    unsigned int port = 0;
    apr_dbd_t *sql = apr_pcalloc(pool, sizeof(apr_dbd_t));
    sql->fldsz = FIELDSIZE;
    sql->conn = mysql_init(sql->conn);
    if ( sql->conn == NULL ) {
        return NULL;
    }
    for (ptr = strchr(params, '='); ptr; ptr = strchr(ptr, '=')) {
        for (key = ptr-1; isspace(*key); --key);
        klen = 0;
        while (isalpha(*key)) {
            /* don't parse backwards off the start of the string */
            if (key == params) {
                --key;
                ++klen;
                break;
            }
            --key;
            ++klen;
        }
        ++key;
        for (value = ptr+1; isspace(*value); ++value);
        vlen = strcspn(value, delims);
        for (i = 0; fields[i].field != NULL; i++) {
            if (!strncasecmp(fields[i].field, key, klen)) {
                fields[i].value = apr_pstrndup(pool, value, vlen);
                break;
            }
        }
        ptr = value+vlen;
    }
    if (fields[4].value != NULL) {
        port = atoi(fields[4].value);
    }
    if (fields[6].value != NULL &&
        !strcmp(fields[6].value, "CLIENT_FOUND_ROWS")) {
        flags |= CLIENT_FOUND_ROWS; /* only option we know */
    }
    if (fields[7].value != NULL) {
        sql->fldsz = atol(fields[7].value);
    }
    if (fields[8].value != NULL) {
         mysql_options(sql->conn, MYSQL_READ_DEFAULT_GROUP, fields[8].value);
    }

#if MYSQL_VERSION_ID >= 50013
    /* the MySQL manual says this should be BEFORE mysql_real_connect */
    mysql_options(sql->conn, MYSQL_OPT_RECONNECT, &do_reconnect);
#endif
    
    real_conn = mysql_real_connect(sql->conn, fields[0].value,
                                   fields[1].value, fields[2].value,
                                   fields[3].value, port,
                                   fields[5].value, flags);

    if(real_conn == NULL) {
        mysql_close(sql->conn);
        return NULL;
    }

#if MYSQL_VERSION_ID >= 50013
    /* Some say this should be AFTER mysql_real_connect */
    mysql_options(sql->conn, MYSQL_OPT_RECONNECT, &do_reconnect);
#endif

    return sql;
}

static apr_status_t dbd_mysql_close(apr_dbd_t *handle)
{
    mysql_close(handle->conn);
    return APR_SUCCESS;
}

static apr_status_t dbd_mysql_check_conn(apr_pool_t *pool,
                                         apr_dbd_t *handle)
{
    return mysql_ping(handle->conn) ? APR_EGENERAL : APR_SUCCESS;
}

static int dbd_mysql_select_db(apr_pool_t *pool, apr_dbd_t* handle,
                               const char* name)
{
    return mysql_select_db(handle->conn, name);
}

static void *dbd_mysql_native(apr_dbd_t *handle)
{
    return handle->conn;
}

static int dbd_mysql_num_cols(apr_dbd_results_t *res)
{
    if (res->statement) {
        return mysql_stmt_field_count(res->statement);
    }
    else {
        return mysql_num_fields(res->res);
    }
}

static int dbd_mysql_num_tuples(apr_dbd_results_t *res)
{
    if (res->random) {
        if (res->statement) {
            return (int) mysql_stmt_num_rows(res->statement);
        }
        else {
            return (int) mysql_num_rows(res->res);
        }
    }
    else {
        return -1;
    }
}

static apr_status_t thread_end(void *data)
{
    mysql_thread_end();
    return APR_SUCCESS;
}

static void dbd_mysql_init(apr_pool_t *pool)
{
    my_init();
    mysql_thread_init();

    /* FIXME: this is a guess; find out what it really does */ 
    apr_pool_cleanup_register(pool, NULL, thread_end, apr_pool_cleanup_null);
}
APU_DECLARE_DATA const apr_dbd_driver_t apr_dbd_mysql_driver = {
    "mysql",
    dbd_mysql_init,
    dbd_mysql_native,
    dbd_mysql_open,
    dbd_mysql_check_conn,
    dbd_mysql_close,
    dbd_mysql_select_db,
    dbd_mysql_transaction,
    dbd_mysql_end_transaction,
    dbd_mysql_query,
    dbd_mysql_select,
    dbd_mysql_num_cols,
    dbd_mysql_num_tuples,
    dbd_mysql_get_row,
    dbd_mysql_get_entry,
    dbd_mysql_error,
    dbd_mysql_escape,
    dbd_mysql_prepare,
    dbd_mysql_pvquery,
    dbd_mysql_pvselect,
    dbd_mysql_pquery,
    dbd_mysql_pselect
};

#endif
