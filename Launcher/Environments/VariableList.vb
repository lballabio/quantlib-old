
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

    Public Class VariableList
        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private variableList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = "Variables"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public ReadOnly Property Variables() As Collection
            Get
                Return variableList_
            End Get
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Public Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub insert(ByVal name As String, ByVal value As String)

            Dim variable As New QuantLibXL.Variable(name, value)
            variableList_.Add(variable, name)

        End Sub

        Public Sub update(ByVal nameOld As String, ByVal nameNew As String, ByVal valueNew As String)

            delete(nameOld)
            insert(nameNew, valueNew)

        End Sub

        Public Sub delete(ByVal name As String)

            variableList_.Remove(name)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        ' serialize() - Read/write this object from/to the given serializer.

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection2(variableList_, "Variable", versionNumber)

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New VariableList
            For Each variable As Variable In variableList_
                Clone.Variables.Add(CType(variable, Variable).Clone, variable.Name)
            Next

        End Function

    End Class

End Namespace