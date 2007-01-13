
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

    Public Class ConsoleWriter
        Implements ISerializer

        ''''''''''''''''''''''''''''''''''''''''''
        ' private members
        ''''''''''''''''''''''''''''''''''''''''''

        Dim indent_ As Long = 0
        Private Const PAD = 15

        Public Sub New()
        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' private properties
        ''''''''''''''''''''''''''''''''''''''''''

        Private ReadOnly Property Indent() As String
            Get
                Indent = Space(indent_)
            End Get
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' public serializer interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serializeObject(ByRef serializable As ISerializable, Optional ByVal className As String = Nothing, Optional ByVal failIfNotFound As Boolean = True) Implements ISerializer.serializeObject
            print("class name", className)
            indent_ += 1
            serializable.serialize(Me)
            indent_ -= 1
        End Sub

        Public Sub serializeObjectCollection(ByRef serializableCollection As Collection, Optional ByVal className As String = Nothing) Implements ISerializer.serializeObjectCollection
            Dim serializable As ISerializable
            For Each serializable In serializableCollection
                serializeObject(serializable, className)
            Next
        End Sub

        Public Sub serializeAttribute(ByRef attr As String, ByVal tag As String) Implements ISerializer.serializeAttribute
            print(tag, attr)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty
            print(tag, prop)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As String(), ByVal tag As String, Optional ByVal defaultValue As String = Nothing) Implements ISerializer.serializeProperty
            print("list name", tag + "s")
            indent_ += 1
            For Each val As String In prop
                print(tag, val)
            Next
            indent_ -= 1
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String) Implements ISerializer.serializeProperty
            print(tag, prop)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String, ByVal defaultValue As Boolean) Implements ISerializer.serializeProperty
            print(tag, prop)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String) Implements ISerializer.serializeProperty
            print(tag, prop)
        End Sub

        Public Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String, ByVal defaultValue As Integer) Implements ISerializer.serializeProperty
            print(tag, prop)
        End Sub

        Sub close() Implements ISerializer.close
        End Sub


        ''''''''''''''''''''''''''''''''''''''''''
        ' private utility functions
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub print(ByVal tag As String, ByVal value As String)
            Console.WriteLine(Indent & tag.PadRight(PAD, " ") & " = " & value)
        End Sub

    End Class

End Namespace
