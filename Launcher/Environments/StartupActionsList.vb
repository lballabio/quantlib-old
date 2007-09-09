
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

Namespace QuantLibXL

    Public Class StartupActionsList
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private startupActionsList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = "StartupActionsList"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public ReadOnly Property StartupActionsList() As Collection
            Get
                Return startupActionsList_
            End Get
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' public interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection(startupActionsList_, "StartupActions", versionNumber)

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Public Sub clear()

            startupActionsList_.Clear()

        End Sub

        Public Sub add(ByVal startupActions As StartupActions, ByVal name As String)

            startupActions.Name = name
            startupActionsList_.Add(startupActions, name)

        End Sub

    End Class

End Namespace
