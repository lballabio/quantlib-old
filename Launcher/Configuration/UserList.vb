
'Copyright (C) 2006 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/reference/license.html>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

Namespace QuantLibXL

    Public Class UserList
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private userList_ As Collection = New Collection
        Private Const USER_NAME = "USERNAME"

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name
            Get
                Name = "Users"
            End Get
            Set(ByVal value As String)
            End Set
        End Property

        Public Sub serialize(ByRef serializer As ISerializer) Implements ISerializable.serialize
            serializer.serializeObjectCollection(userList_, "User")
        End Sub

        Public Sub validate()
            Dim userName As String
            userName = Environ(USER_NAME)
            If Len(userName) < 1 Then
                Throw New Exception("Unable to retrieve the value of " _
                    & "environment variable " & USER_NAME)
            End If

            Dim serialNumber As String = getSerialNumber()
            If userList_.Contains(userName) Then
                If userList_(userName).validateSerialNumber(serialNumber) Then Exit Sub
            End If

            Throw New Exception( _
                "Sorry - you aren't authorized to use QuantLibXL." & vbCrLf & vbCrLf _
                & "Please ask QuantLibXL support to grant access to:" & vbCrLf & vbCrLf _
                & "    user name : " & userName & vbCrLf _
                & "    hard disk serial number : " & serialNumber & vbCrLf)

        End Sub

    End Class

End Namespace
