
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

    Public Class User
        Implements ISerializable

        Private name_ As String
        Private serialNumbers_() As String

        Public Property Name() As String Implements ISerializable.Name
            Get
                Name = name_
            End Get
            Set(ByVal value As String)
                name_ = value
            End Set
        End Property

        Public Sub serialize(ByRef serializer As ISerializer) Implements ISerializable.serialize
            serializer.serializeAttribute(name_, "name")
            serializer.serializeProperty(serialNumbers_, "serialNumber")
        End Sub

        Public Function validateSerialNumber(ByVal serialNumber As String) As Boolean
            For Each val As String In serialNumbers_
                If val = serialNumber Then
                    validateSerialNumber = True
                    Exit Function
                End If
            Next
        End Function

    End Class

End Namespace
