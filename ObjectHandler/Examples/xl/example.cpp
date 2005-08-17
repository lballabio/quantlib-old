#include <windows.h>
#define bool boolean
#include <xlcall.h>
#undef bool
#include <framewrk.hpp>
#include <fstream>
#include <oh/objhandler.hpp>
#include <objectfoo.hpp>

using namespace std;
using namespace ObjHandler;

#define DLLEXPORT extern "C" __declspec(dllexport)
#define XL_MAX_STR_LEN 255

// suppress VC8 'strncpy deprecated' warning
#if defined BOOST_MSVC
#pragma warning(disable : 4996)
#endif

DLLEXPORT int xlAutoOpen() {
    static XLOPER xDll;
    Excel(xlGetName, &xDll, 0);

    Excel(xlfRegister, 0, 7, &xDll,
        TempStrNoSize("\x07""makeFoo"),          // function code name
        TempStrNoSize("\x04""CCCN"),             // parameter codes
        TempStrNoSize("\x10""EXAMPLE_MAKE_FOO"), // function display name
        TempStrNoSize("\x0A""handle,s,i"),       // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),                // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x07""Example"));         // function category

    Excel(xlfRegister, 0, 7, &xDll,
        TempStrNoSize("\x09""updateFoo"),        // function code name
        TempStrNoSize("\x04""LCCN"),             // parameter codes
        TempStrNoSize("\x12""EXAMPLE_UPDATE_FOO"),// function display name
        TempStrNoSize("\x0A""handle,s,i"),       // comma-delimited list of parameters
        TempStrNoSize("\x01""1"),                // function type (0 = hidden function, 1 = worksheet function, 2 = command macro)
        TempStrNoSize("\x07""Example"));         // function category

    Excel(xlFree, 0, 1, &xDll);
    return 1;
}

DLLEXPORT char* makeFoo(char *handle, char *s, long *i) {
    try {
        obj_ptr objectPointer(new ObjectFoo(s, *i));
        storeObject(handle, objectPointer);
        static char ret[XL_MAX_STR_LEN];
        int len = __min(XL_MAX_STR_LEN - 1, strlen(handle));
        strncpy(ret, handle, len);
        ret[len] = 0;
        return ret;
    } catch (const std::exception &e) {
        logMessage(std::string("Error: EXAMPLE_MAKE_FOO: ") + e.what(), 2);
        return 0;
    }
}

DLLEXPORT short int *updateFoo(char *handle, char *s, long *i) {
    try {
        updateFoo(handle, s, *i);
        static short int ret = TRUE;
        return &ret;
    } catch (const std::exception &e) {
        logMessage(std::string("Error: EXAMPLE_UPDATE_FOO: ") + e.what(), 2);
        return 0;
    }
}