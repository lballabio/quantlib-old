
'Copyright (C) 2007 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/license.shtml>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

Imports System

Namespace QuantLibXL

    Public Class DomainList
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private domainList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = "Domains"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection(domainList_, "Domain", versionNumber)

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Public Sub validate(ByVal environmentName As String)

            Dim userName As String = System.Environment.UserName
            Dim domainName As String = System.Environment.UserDomainName
            Dim serialNumber As String = getSerialNumber()

            If domainList_.Contains(domainName) Then
                If domainList_(domainName).validate(userName, serialNumber) Then
                    Exit Sub
                End If
            End If

            Throw New Exception( _
                "Sorry - you aren't authorized to use QuantLibXL environment" _
                & vbCrLf & "'" & environmentName & "'." & vbCrLf & vbCrLf _
                & "Please ask QuantLibXL support to grant access to:" & vbCrLf & vbCrLf _
                & "    user name : " & userName & vbCrLf _
                & "    domain name : " & domainName & vbCrLf _
                & "    hard disk serial number : " & serialNumber & vbCrLf)

        End Sub

    End Class

End Namespace
