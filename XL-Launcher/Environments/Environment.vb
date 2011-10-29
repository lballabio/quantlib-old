
Namespace XL_Launcher

    ''' <summary>
    ''' A collection of Framework objects comprising all of the resources to be loaded
    ''' into a given instance of Excel.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Environment

        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private authenticationFile_ As String = ""
        Private sessionFilePath_ As String = ""
        Private frameworkList_ As Collection = New Collection

        Private Const VERSION_NUMBER As Integer = 1
        Private Const XL_LAUNCHER_PATH As String = "XL_LAUNCHER_PATH"

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property Frameworks() As Collection

            Get
                Frameworks = frameworkList_
            End Get

        End Property

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "EnvironmentConfig"
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

        Public ReadOnly Property AuthenticationFile() As String

            Get
                AuthenticationFile = authenticationFile_
            End Get

        End Property

        Public ReadOnly Property SessionFilePath() As String

            Get
                SessionFilePath = sessionFilePath_
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New Environment(name_, "", sessionFilePath_)
            For Each f As Framework In frameworkList_
                Clone.Frameworks.Add(CType(f, Framework).Clone, f.Name)
            Next

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name", "")
            serializer.serializeProperty(authenticationFile_, "AuthenticationFile", "")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

        End Sub

        Public Sub New(ByVal name As String, ByVal userConfigPath As String, ByVal sessionFilePath As String)

            name_ = name
            authenticationFile_ = userConfigPath
            sessionFilePath_ = sessionFilePath

        End Sub

        Public Sub Load(ByVal path As String)

            For Each fileNameFull As String In My.Computer.FileSystem.GetFiles(path)

                Dim fileName As String = My.Computer.FileSystem.GetFileInfo(fileNameFull).Name

                If ".xml" = Microsoft.VisualBasic.Right(fileName, 4) And _
                "_EnvironmentConfig.xml" <> fileName Then

                    Dim f As Framework = Serialization.loadObject(fileNameFull, "Framework")
                    f.setFileName(Microsoft.VisualBasic.Left(fileName, fileName.Length - 4))
                    f.initialize()
                    frameworkList_.Add(f, f.Name)

                End If

            Next

        End Sub

        Public Sub defaultName(ByVal defaultName As String)

            If 0 = name_.Length Then name_ = defaultName

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Public Methods
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function nameToFramework(ByVal frameworkName As String) As Framework

            If frameworkList_.Contains(frameworkName) Then
                nameToFramework = frameworkList_(frameworkName)
            Else
                Throw New Exception("No framework with name '" & frameworkName & "'")
            End If

        End Function

        Public Function nameInUse(ByVal frameworkName As String) As Boolean

            nameInUse = frameworkList_.Contains(frameworkName)

        End Function

        Public Sub generateSessionFile(ByVal globalAddins As Collection, ByVal sessionFileDir As String)

            ' Derive a name for the XML file.  We must guard against race
            ' conditions, i.e. two instances of the Launcher creating two
            ' temp files with the same name.  So the name is in the format
            ' launch.xxx.yyy.xml where xxx is this process ID
            ' and yyy is the current time in subseconds.

            System.IO.Directory.CreateDirectory(sessionFileDir)

            sessionFilePath_ = sessionFileDir & _
                "XL-Launcher." & GetCurrentProcessId & _
                "." & DateTime.Now.Ticks & ".xml"

            ' Create the Message object

            Dim m As Message = New Message(frameworkList_, globalAddins)

            ' Write this environment object to the temp file

            Serialization.saveObject(sessionFilePath_, m, "Environment")

        End Sub

        Public Sub launch(ByVal excelPath As String, ByVal sessionFileDir As String, _
            ByVal launcherXlaPath As String, ByVal globalAddins As Collection)

            validateExcelPath(excelPath)
            'validate()

            ' Generate the session file

            generateSessionFile(globalAddins, sessionFileDir)

            ' Set the environment variable equal to the path of the session file

            System.Environment.SetEnvironmentVariable(XL_LAUNCHER_PATH, sessionFilePath_)

            ' Set any other variables specified for this environment

            For Each f As Framework In frameworkList_

                For Each v As EnvironmentVariable In f.EnvironmentVariables

                    System.Environment.SetEnvironmentVariable(v.Name, v.Value)

                Next

            Next f

            ' Spawn the subprocess

            Dim commandLine As String
            commandLine = """" & excelPath & """ /e """ & launcherXlaPath & """"
            Shell(commandLine, AppWinStyle.NormalFocus)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' user authentication
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function authenticate() As Boolean

            Try

                If 0 = authenticationFile_.Length Then

                    authenticate = True
                    Exit Function

                End If

                If Not System.IO.File.Exists(authenticationFile_) Then Exit Function

                Dim a As Authentication = Serialization.loadObject( _
                    authenticationFile_, "Authentication")

                authenticate = a.authenticate()

            Catch ex As Exception

                'XL_Launcher.Errors.displayError("Error authenticating environment '" & name_ & "'", ex)
                authenticate = False

            End Try

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Private Methods
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub validateExcelPath(ByVal excelPath As String)

            If excelPath.Length > 0 Then
                If Not System.IO.File.Exists(excelPath) Then
                    Throw New Exception("Error: this application is configured to load Excel " & _
                        "from the following location:" & vbCrLf & vbCrLf & excelPath & _
                        vbCrLf & vbCrLf & "The path appears to be invalid." & vbCrLf & vbCrLf & _
                        "Go to the 'Paths' tab and set the path to Excel.")
                End If
            Else
                Throw New Exception("The path to Excel has not been specified." & _
                    vbCrLf & "Go to the 'Paths' tab and set the path to Excel.")
            End If

        End Sub

    End Class

End Namespace
