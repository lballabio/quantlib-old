
'Copyright (C) 2006, 2007 Eric Ehlers

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

    Public Class EnvironmentList
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private environmentList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = "Environments"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public ReadOnly Property Environments() As Collection
            Get
                Return environmentList_
            End Get
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' public interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection(environmentList_, "Environment", versionNumber)

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Public Sub validate()

            If environmentList_.Count < 1 Then
                Throw New Exception("No environments are configured.")
            End If

        End Sub

        Public Sub setDotNetParameters()

            Dim env As QuantLibXL.Environment
            For Each env In environmentList_
                env.setDotNetParameters()
            Next

        End Sub

        Public Function nameToEnvironment(ByVal environmentName As String) As Environment

            If environmentList_.Contains(environmentName) Then
                nameToEnvironment = environmentList_(environmentName)
            Else
                Throw New Exception("No environment with name " & environmentName)
            End If

        End Function

        Public Function createEnvironment(ByVal environmentName As String) As Environment

            createEnvironment = New Environment
            createEnvironment.Name = environmentName
            environmentList_.Add(createEnvironment, environmentName)

        End Function

        Public Function copyEnvironment(ByVal environment As Environment, ByVal copyName As String) As Environment

            copyEnvironment = CType(environment, Environment).Clone()
            copyEnvironment.Name = copyName
            environmentList_.Add(copyEnvironment, copyName)

        End Function

        Public Sub deleteEnvironment(ByVal environmentName As String)

            If environmentList_.Contains(environmentName) Then
                environmentList_.Remove(environmentName)
            Else
                Throw New Exception("Attempt to delete nonexistent environment " & environmentName)
            End If

        End Sub

        Public Function nameInUse(ByVal testName As String) As Boolean

            nameInUse = environmentList_.Contains(testName)

        End Function

        Public Function deriveNewName() As String

            Dim baseCount As Integer = 1
            Do
                deriveNewName = "Environment " & baseCount
                If nameInUse(deriveNewName) Then
                    baseCount = baseCount + 1
                Else
                    Exit Function
                End If
            Loop

        End Function

        Public Function deriveCopyName(ByVal text As String) As String

            deriveCopyName = "Copy of " & text
            If nameInUse(deriveCopyName) Then
                Dim baseCount As Integer = 2
                Do
                    deriveCopyName = "Copy (" & baseCount & ") of " & text
                    If nameInUse(deriveCopyName) Then
                        baseCount = baseCount + 1
                    Else
                        Exit Function
                    End If
                Loop
            End If

        End Function

        Public Sub renameEnvironment(ByVal oldName As String, ByVal newName As String)

            If environmentList_.Contains(oldName) Then
                Dim e As Environment
                e = environmentList_(oldName)
                environmentList_.Add(e, newName)
                environmentList_.Remove(oldName)
                e.Name = newName
            Else
                Throw New Exception("Attempt to rename nonexistent environment " & oldName)
            End If

        End Sub

    End Class

End Namespace
