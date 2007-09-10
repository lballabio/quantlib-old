
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

    Public Class AddinList
        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Constants
        ''''''''''''''''''''''''''''''''''''''''''

        ' The maximum number of XLLs that the Launcher can instruct the Framework to load.
        ' At present this value is limited to 10 only because the addin names
        ' are written to registry keys in the format Addin0, Addin1, ..., AddinN
        ' and the list doesn't sort correctly for values of N greater than 9.

        Public Const MAX_ADDIN_COUNT As Integer = 10

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private addinList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = "AddinList"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public ReadOnly Property Variables() As Collection
            Get
                Return addinList_
            End Get
        End Property

        Public Property Addins() As Collection

            Get
                Return addinList_
            End Get

            Set(ByVal value As Collection)
                addinList_ = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Public Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub insert(ByVal path As String, ByVal before As Integer)

            If addinList_.Count >= MAX_ADDIN_COUNT Then
                Throw New Exception("You cannot insert another addin into the list" _
                 & " because the list currently contains " & addinList_.Count _
                 & " items which is the maximum supported by the Launcher.")
            End If

            addinList_.Add(New Addin(path), Before:=before)

        End Sub

        Public Sub update(ByVal pathOld As String, ByVal pathNew As String)

            Dim index As Integer = 1
            For Each addin As Addin In addinList_
                If addin.Path = pathOld Then
                    addin.Path = pathNew
                    Exit Sub
                End If
                index = index + 1
            Next

            Throw New Exception("Error updating addin '" & pathOld _
                & " - no addin with that name.")

        End Sub

        Public Sub delete(ByVal path As String)

            Dim index As Integer = 1
            For Each addin As Addin In addinList_
                If addin.Path = path Then
                    addinList_.Remove(index)
                    Exit Sub
                End If
                index = index + 1
            Next

            Throw New Exception("Error deleting addin '" & path _
                & "' - no addin with that name.")

        End Sub

        Public Sub up(ByVal index As Integer)

            Dim addin As Addin = addinList_.Item(index + 1)
            addinList_.Remove(index + 1)
            addinList_.Add(addin, Before:=index)

        End Sub

        Public Sub down(ByVal index As Integer)

            Dim addin As Addin = addinList_.Item(index + 1)
            addinList_.Remove(index + 1)
            addinList_.Add(addin, Before:=index + 2)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        ' serialize() - Read/write this object from/to the given serializer.

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            If versionNumber > 9 Then
                serializer.serializeObjectList(addinList_, "Addin", versionNumber)
            Else
                For Each addin As Addin In addinList_
                    serializer.serializeProperty(addin.Path, "Addin")
                Next
            End If

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New AddinList
            For Each addin As Addin In addinList_
                Clone.Addins.Add(CType(addin, Addin).Clone, addin.Name)
            Next

        End Function

    End Class

End Namespace