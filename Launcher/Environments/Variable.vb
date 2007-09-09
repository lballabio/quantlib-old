
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Class Variable - Encapsulate the state and behavior relating to
' an environment variable - set by the Launcher for use by the Framework
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Namespace QuantLibXL

    Public Class Variable
        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private value_ As String

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = name_
            End Get

            Set(ByVal value As String)
                name_ = value
            End Set

        End Property

        Public Property Value() As String

            Get
                Value = value_
            End Get

            Set(ByVal value As String)
                value_ = value
            End Set

        End Property

        Public Sub New(ByVal name As String, ByVal value As String)

            name_ = name
            value_ = value

        End Sub

        Public Sub New()
        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        ' serialize() - Read/write this object from/to the given serializer.

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            ' Serialize some XML that looks like this:
            ' <Variable>
            '   <Name>FOO_DIR</Name>
            '   <Value>C:\Path\To\Foo</Value>
            ' </Variable
            serializer.serializeProperty(name_, "Name")
            serializer.serializeProperty(value_, "Value")

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2

            ' Serialize a registry key that looks like this:
            ' FOO_DIR       C:\Path\To\Foo
            serializer.serializeAttribute(value_, name_)

        End Sub

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = Me.MemberwiseClone()

        End Function

    End Class

End Namespace