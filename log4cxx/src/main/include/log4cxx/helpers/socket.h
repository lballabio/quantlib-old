/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef _LOG4CXX_HELPERS_SOCKET_H
#define _LOG4CXX_HELPERS_SOCKET_H

#include <log4cxx/logstring.h>
#include <log4cxx/helpers/socketimpl.h>

namespace log4cxx
{
        namespace helpers
        {
                class ServerSocker;


                /**
                <p>This class implements client sockets (also called just "sockets"). A socket
                is an endpoint for communication between two machines.
                <p>The actual work of the socket is performed by an instance of the SocketImpl
                class. An application, by changing the socket factory that creates the socket
                implementation, can configure itself to create sockets appropriate to the
                local firewall.
                */
                class LOG4CXX_EXPORT Socket : public helpers::ObjectImpl
                {
                friend class ServerSocket;
                protected:
                        /** Creates an unconnected socket.
                        */
                        Socket();

                public:
                        DECLARE_ABSTRACT_LOG4CXX_OBJECT(Socket)
                        BEGIN_LOG4CXX_CAST_MAP()
                                LOG4CXX_CAST_ENTRY(Socket)
                        END_LOG4CXX_CAST_MAP()

                        /** Creates a stream socket and connects it to the specified port
                        number at the specified IP address.
                        */
                        Socket(InetAddressPtr address, int port);

                        /** Creates a socket and connects it to the specified remote
                        address on the specified remote port.
                        */
                        Socket(InetAddressPtr address, int port,
                                InetAddressPtr localAddr, int localPort);

                protected:
                        /** Creates an unconnected Socket
                        with a user-specified SocketImpl.
                        */
                        Socket(SocketImplPtr impl);

                public:
                        /** Creates a stream socket and connects it to the specified
                        port number on the named host.
                        */
                        Socket(const LogString& host, int port);

                        /**  Creates a socket and connects it to the specified remote
                        host on the specified remote port.
                        */
                        Socket(const LogString& host, int port,
                                InetAddressPtr localAddr, int localPort);

                        size_t read(void * buf, size_t len) const
                                { return socketImpl->read(buf, len); }

                        size_t write(const void * buf, size_t len)
                                { return socketImpl->write(buf, len); }

                        /** Closes this socket. */
                        void close()
                                { socketImpl->close(); }

                        /** Returns the value of this socket's address field. */
                        inline InetAddressPtr getInetAddress() const
                                { return socketImpl->getInetAddress(); }

                        /** Returns the value of this socket's localport field. */
                        inline int getLocalPort() const
                                { return socketImpl->getLocalPort(); }

                        /** Returns the value of this socket's port field. */
                        inline int getPort() const
                                { return socketImpl->getPort(); }

                private:
                        Socket(const Socket&);
                        Socket& operator=(const Socket&);
                        SocketImplPtr socketImpl;
                };
                
                LOG4CXX_PTR_DEF(Socket);
                
        } // namespace helpers
} // namespace log4cxx

#endif // _LOG4CXX_HELPERS_SOCKET_H
