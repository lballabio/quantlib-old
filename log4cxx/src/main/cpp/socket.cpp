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
#include <log4cxx/logstring.h>
#include <log4cxx/helpers/socket.h>
#include <log4cxx/helpers/loglog.h>

using namespace log4cxx;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(Socket)

/** Creates an unconnected socket.
*/
Socket::Socket() : socketImpl()
{
}

/** Creates a stream socket and connects it to the specified port
number at the specified IP address.
*/
Socket::Socket(InetAddressPtr address, int port) : socketImpl(new SocketImpl())
{
   socketImpl->create(true);
   socketImpl->connect(address, port);
}

/** Creates a socket and connects it to the specified remote
address on the specified remote port.
*/
Socket::Socket(InetAddressPtr address, int port,
   InetAddressPtr localAddr, int localPort) : socketImpl(new SocketImpl())
{
   socketImpl->create(true);
   socketImpl->connect(address, port);
   socketImpl->bind(localAddr, localPort);
}

/** Creates an unconnected Socket
with a user-specified SocketImpl.
*/
Socket::Socket(SocketImplPtr impl) : socketImpl(impl)
{
}


/** Creates a stream socket and connects it to the specified
port number on the named host.
*/
Socket::Socket(const LogString& host, int port)
   : socketImpl(new SocketImpl())
{
   socketImpl->create(true);
   socketImpl->connect(host, port);
}

/**  Creates a socket and connects it to the specified remote
host on the specified remote port.
*/
Socket::Socket(const LogString& host, int port,
   InetAddressPtr localAddr, int localPort)
        : socketImpl(new SocketImpl())
{
   socketImpl->create(true);
   socketImpl->connect(host, port);
   socketImpl->bind(localAddr, localPort);
}

