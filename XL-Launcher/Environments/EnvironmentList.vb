
Namespace XL_Launcher

    ''' <summary>
    ''' A collection of Environment objects.  In practice the application uses
    ''' two EnvironmentList objects, one for Pre Configured Environments and the
    ''' other for User Configured Environments.  Each of these is displayed in
    ''' a ListBox object in the main UI.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class EnvironmentList

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Public Const PRECONFIGURED As String = "PreConfigured"
        Public Const USERCONFIGURED As String = "UserConfigured"

        Private environmentList_ As Collection = New Collection
        Private excelPath_ As String
        Private globalAddins_ As Collection = New Collection
        Private selectedEnvironmentType_ As String = Nothing
        Private selectedEnvironmentName_ As String = Nothing
        Private selectedFrameworkName_ As String = Nothing
        Private environmentTabIndex_ As Integer = -1
        Private frameworkTabIndex_ As Integer = -1
        Private loadError_ As Boolean = False

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property Environments() As Collection

            Get
                Return environmentList_
            End Get

        End Property

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "LauncherConfig"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = nodeName
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Property ExcelPath() As String

            Get
                ExcelPath = excelPath_
            End Get

            Set(ByVal value As String)
                excelPath_ = value
            End Set

        End Property

        Public ReadOnly Property GlobalAddins() As Collection

            Get
                Return globalAddins_
            End Get

        End Property

        Public Property SelectedEnvironmentType() As String

            Get
                SelectedEnvironmentType = selectedEnvironmentType_
            End Get

            Set(ByVal value As String)
                selectedEnvironmentType_ = value
            End Set

        End Property

        Public Property SelectedEnvironmentName() As String

            Get
                SelectedEnvironmentName = selectedEnvironmentName_
            End Get

            Set(ByVal value As String)
                selectedEnvironmentName_ = value
            End Set

        End Property

        Public Property SelectedFrameworkName() As String

            Get
                SelectedFrameworkName = selectedFrameworkName_
            End Get

            Set(ByVal value As String)
                selectedFrameworkName_ = value
            End Set

        End Property

        Public Property EnvironmentTabIndex() As Integer

            Get
                EnvironmentTabIndex = environmentTabIndex_
            End Get

            Set(ByVal value As Integer)
                environmentTabIndex_ = value
            End Set

        End Property

        Public Property FrameworkTabIndex() As Integer

            Get
                FrameworkTabIndex = frameworkTabIndex_
            End Get

            Set(ByVal value As Integer)
                frameworkTabIndex_ = value
            End Set

        End Property

        Public ReadOnly Property LoadError() As Boolean

            Get
                Return loadError_
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeProperty(excelPath_, "ExcelPath")
            serializer.serializeProperty(selectedEnvironmentType_, "SelectedEnvironmentType", "")
            serializer.serializeProperty(selectedEnvironmentName_, "SelectedEnvironmentName", "")
            serializer.serializeProperty(selectedFrameworkName_, "SelectedFrameworkName", "")
            serializer.serializeProperty(environmentTabIndex_, "EnvironmentTabIndex", "0")
            serializer.serializeProperty(frameworkTabIndex_, "FrameworkTabIndex", "0")
            serializer.serializeObjectCollection(globalAddins_, "GlobalAddin", versionNumber, False)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Methods
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub loadEnvironment(ByVal path As String, ByVal envName As String)

            Dim e As Environment = Nothing
            Dim envCfgPath As String = path & "\_EnvironmentConfig.xml"

            If System.IO.File.Exists(envCfgPath) Then

                e = loadObject(envCfgPath, "EnvironmentConfig")
                If Not e.authenticate Then Exit Sub
                e.defaultName(envName)

            Else

                e = New Environment
                e.Name = envName

            End If

            e.Load(path)
            environmentList_.Add(e, e.Name)

        End Sub

        Public Sub loadEnvironments(ByVal configPath As String)

            For Each envPath As String In My.Computer.FileSystem.GetDirectories(configPath)

                Dim f As System.IO.FileInfo = My.Computer.FileSystem.GetFileInfo(envPath)
                Dim folderName As String = f.Name
                If "." <> Microsoft.VisualBasic.Left(folderName, 1) Then

                    Try

                        loadEnvironment(envPath, folderName)

                    Catch ex As Exception

                        loadError_ = True
                        MsgBox("Error loading environment '" & folderName & "' - environment ignored." & vbCrLf & vbCrLf & _
                            "Path:" & vbCrLf & vbCrLf & _
                            vbTab & envPath & vbCrLf & vbCrLf & _
                            "Error message:" & vbCrLf & vbCrLf & _
                            ex.Message, _
                            MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, ERROR_MESSAGE)

                    End Try

                End If

            Next

        End Sub

        Public Function nameToEnvironment(ByVal environmentName As String) As Environment

            If environmentList_.Contains(environmentName) Then
                nameToEnvironment = environmentList_(environmentName)
            Else
                Throw New Exception("No environment with name '" & environmentName & "'")
            End If

        End Function

        Public Function nameInUse(ByVal environmentName As String) As Boolean

            nameInUse = environmentList_.Contains(environmentName)

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

        Public Function copyEnvironment(ByVal environment As Environment, ByVal copyName As String) As Environment

            copyEnvironment = CType(environment, Environment).Clone()
            copyEnvironment.Name = copyName
            environmentList_.Add(copyEnvironment, copyName)

        End Function

        Public Sub renameEnvironment(ByVal oldName As String, ByVal newName As String)

            If environmentList_.Contains(oldName) Then

                Dim e As Environment = environmentList_(oldName)
                e.Name = newName
                environmentList_.Remove(oldName)
                environmentList_.Add(e, newName)

            Else

                Throw New Exception("Attempt to rename nonexistent environment '" & oldName & "'")

            End If

        End Sub

        Public Sub deleteEnvironment(ByVal environmentName As String)

            If environmentList_.Contains(environmentName) Then
                environmentList_.Remove(environmentName)
            Else
                Throw New Exception("Attempt to delete nonexistent environment " & environmentName)
            End If

        End Sub

        Public Sub overrideParameters(ByVal l As EnvironmentList)

            ExcelPath = l.ExcelPath

            For Each e As Environment In l.Environments

                If nameInUse(e.Name) Then

                    Dim e2 As Environment = nameToEnvironment(e.Name)

                    For Each f As Framework In e.Frameworks

                        If e2.nameInUse(f.Name) Then

                            Dim f2 As Framework = e2.nameToFramework(f.Name)

                            For Each sp As StartupParameter In f.StartupParameterList

                                If f2.nameInUse(sp.parameterName) Then

                                    Dim sp2 As StartupParameter = f2.nameToStartupParameter(sp.parameterName)

                                    If sp.typeName = sp2.typeName Then

                                        sp2.displayValue = sp.displayValue

                                    End If

                                End If

                            Next sp

                        End If

                    Next f

                End If

            Next e

            For Each g As GlobalAddin In l.GlobalAddins

                If globalAddins_.Contains(g.Name) Then

                    Dim g2 As GlobalAddin = globalAddins_(g.Name)
                    g2.Enabled = g.Enabled
                    g2.Path = g.Path

                End If

            Next g

        End Sub

    End Class

End Namespace
