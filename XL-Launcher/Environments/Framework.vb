
Imports System.Deployment.Application

Namespace XL_Launcher

    ''' <summary>
    ''' A collection of Components (Addins) which together comprise an
    ''' Excel VBA application.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Framework

        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private displayName_ As String
        Private fileName_ As String
        Private groupBoxes_ As Collection = New Collection
        Private startupParameterList_ As Collection = New Collection
        Private componentList_ As Collection = New Collection
        Private environmentVariables_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Framework"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = name_
            End Get

            Set(ByVal value As String)
                name_ = value
            End Set

        End Property

        Public ReadOnly Property DisplayName() As String

            Get
                DisplayName = displayName_
            End Get

        End Property

        Public ReadOnly Property FileName() As String

            Get
                FileName = fileName_
            End Get

        End Property

        Public ReadOnly Property StartupParameterList() As Collection

            Get
                StartupParameterList = startupParameterList_
            End Get

        End Property

        Public ReadOnly Property GroupBoxes() As Collection

            Get
                GroupBoxes = groupBoxes_
            End Get

        End Property

        Public ReadOnly Property ComponentList() As Collection

            Get
                ComponentList = componentList_
            End Get

        End Property

        Public ReadOnly Property EnvironmentVariables() As Collection

            Get
                Return environmentVariables_
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New Framework(name_, displayName_, fileName_)
            For Each g As GroupBox In groupBoxes_
                Clone.GroupBoxes.Add(CType(g, GroupBox).Clone, g.Name)
            Next
            For Each c As Component In componentList_
                Clone.ComponentList.Add(CType(c, Component).Clone, c.Name)
            Next
            For Each v As EnvironmentVariable In environmentVariables_
                Clone.EnvironmentVariables.Add(v, v.Name)
            Next

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name", "")
            serializer.serializeAttribute(displayName_, "displayName", "")
            serializer.serializeObjectCollection(groupBoxes_, "GroupBox", versionNumber, False, True, "StartupParameters")
            serializer.serializeObjectCollection(componentList_, "Component", versionNumber, False)
            serializer.serializeObjectCollection(environmentVariables_, "EnvironmentVariable", versionNumber, False)

        End Sub

        '''''''''''''''''''''''''''''''''''''''''''
        '' Initialization
        '''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal name As String, ByVal displayName As String, ByVal fileName As String)

            name_ = name
            displayName_ = displayName
            fileName_ = fileName

        End Sub

        Public Sub New()

        End Sub

        Public Sub initialize()

            For Each g As GroupBox In groupBoxes_

                For Each s As StartupParameter In g.StartupParameterList

                    startupParameterList_.Add(s)

                Next s

            Next g

        End Sub

        Public Sub setFileName(ByVal fileName As String)

            fileName_ = fileName
            If 0 = name_.Length Then name_ = fileName
            If 0 = displayName_.Length Then displayName_ = fileName

        End Sub
        '''''''''''''''''''''''''''''''''''''''''''
        '' Methods
        '''''''''''''''''''''''''''''''''''''''''''

        Public Function componentNameInUse(ByVal componentName As String) As Boolean

            componentNameInUse = componentList_.Contains(componentName)

        End Function

        Public Function nameInUse(ByVal startupParameterName As String) As Boolean

            nameInUse = startupParameterList_.Contains(startupParameterName)

        End Function

        Public Function nameToStartupParameter(ByVal startupParameterName As String) As StartupParameter

            If startupParameterList_.Contains(startupParameterName) Then
                nameToStartupParameter = startupParameterList_(startupParameterName)
            Else
                Throw New Exception("No startup parameter with name " & startupParameterName)
            End If

        End Function

        Public Sub addComponent(ByVal filename As String, ByVal index As Integer)

            componentList_.Add(New Component(filename), filename, index)

        End Sub

        Public Sub renameComponent(ByVal pathOld As String, ByVal pathNew As String)

            Dim index As Integer = 1
            For Each c As Component In componentList_
                If c.path = pathOld Then
                    c.path = pathNew
                    componentList_.Remove(index)
                    componentList_.Add(c, pathNew, index)
                    Exit Sub
                End If
                index = index + 1
            Next

            Throw New Exception("Error renaming component '" & pathOld _
                & " - no component with that name.")

        End Sub

        Public Sub deleteComponent(ByVal path As String)

            Dim index As Integer = 1
            For Each c As Component In componentList_
                If c.path = path Then
                    componentList_.Remove(index)
                    Exit Sub
                End If
                index = index + 1
            Next

            Throw New Exception("Error deleting component '" & path _
                & "' - no component with that name.")

        End Sub

        Public Sub moveComponentUp(ByVal index As Integer)

            Dim c As Component = componentList_.Item(index + 1)
            componentList_.Remove(index + 1)
            componentList_.Add(c, Before:=index)

        End Sub

        Public Sub moveComponentDown(ByVal index As Integer)

            Dim c As Component = componentList_.Item(index + 1)
            componentList_.Remove(index + 1)
            componentList_.Add(c, Before:=index + 2)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Environment Variables
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub insertVariable(ByVal name As String, ByVal value As String)

            Dim variable As New EnvironmentVariable(name, value)
            environmentVariables_.Add(variable, name)

        End Sub

        Public Sub updateVariable(ByVal nameOld As String, ByVal nameNew As String, ByVal valueNew As String)

            deleteVariable(nameOld)
            insertVariable(nameNew, valueNew)

        End Sub

        Public Sub deleteVariable(ByVal name As String)

            environmentVariables_.Remove(name)

        End Sub

    End Class

End Namespace
