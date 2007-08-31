
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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Class Environment - Encapsulate the state and behavior relating to
' a running instance of the QuantLibXL Framework.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Imports System.Deployment.Application

Namespace QuantLibXL

    Public Class Environment
        Implements ICloneable
        Implements ISerializable

        Private Declare Function GetCurrentProcessId Lib "kernel32" () As Long

        ' Environment variable QUANTLIBXL_LAUNCHx informs the Framework of the location
        ' of the configuration file, where x is the Framework version number.

        Private Const QUANTLIBXL_LAUNCH8 As String = "QUANTLIBXL_LAUNCH8"
        Private Const QUANTLIBXL_LAUNCH9 As String = "QUANTLIBXL_LAUNCH9"
        Private Const QUANTLIBXL_LAUNCH10 As String = "QUANTLIBXL_LAUNCH10"

        ' The maximum number of XLLs that the Launcher can instruct the Framework to load.
        ' At present this value is limited to 10 only because the addin names
        ' are written to registry keys in the format Addin0, Addin1, ..., AddinN
        ' and this list doesn't sort correctly for values of N greater than 9.

        Public Const MAX_ADDIN_COUNT As Integer = 10

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String

        ' paths

        Private frameworkName_ As String = ""
        Private frameworkVersion_ As Integer = 0
        Private workbooks_ As String = ""
        Private addinList_() As String
        Private helpPath_ As String = ""
        Private xmlPath_ As String = ""
        Private userConfig_ As String = ""
        Private feedUse_ As String = ""

        ' User authentication

        Private userAuthenticated_ As Boolean = False

        ' Startup actions

        Private startupActions_ As QuantLibXL.StartupActions = New QuantLibXL.StartupActions

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

        Public Property FrameworkName() As String
            Get
                FrameworkName = frameworkName_
            End Get

            Set(ByVal value As String)
                frameworkName_ = value
            End Set

        End Property

        Public Property FrameworkVersion() As Integer
            Get
                FrameworkVersion = frameworkVersion_
            End Get

            Set(ByVal value As Integer)
                frameworkVersion_ = value
            End Set

        End Property

        Public Property Workbooks() As String

            Get
                Workbooks = workbooks_
            End Get

            Set(ByVal value As String)
                workbooks_ = value
            End Set

        End Property

        Public Property AddinList() As String()

            Get
                AddinList = addinList_
            End Get

            Set(ByVal value() As String)
                addinList_ = value
            End Set

        End Property

        Public Property HelpPath() As String

            Get
                HelpPath = helpPath_
            End Get

            Set(ByVal value As String)
                helpPath_ = value
            End Set

        End Property

        Public Property XmlPath() As String

            Get
                XmlPath = xmlPath_
            End Get

            Set(ByVal value As String)
                xmlPath_ = value
            End Set

        End Property

        Public Property UserConfig() As String

            Get
                UserConfig = userConfig_
            End Get

            Set(ByVal value As String)
                userConfig_ = value
            End Set

        End Property

        Public Property StartupActions() As QuantLibXL.StartupActions

            Get
                StartupActions = startupActions_
            End Get

            Set(ByVal value As QuantLibXL.StartupActions)
                startupActions_ = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        ' serialize() - Read/write this object from/to the given serializer.

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeProperty(frameworkName_, "Framework")
            serializer.serializeProperty(frameworkVersion_, "FrameworkVersion")
            serializer.serializeProperty(workbooks_, "Workbooks")
            serializer.serializeProperty(helpPath_, "HelpFile")
            serializer.serializeProperty(xmlPath_, "FunctionMetadata")
            serializer.serializeProperty(userConfig_, "UserConfigurationFile")
            serializer.serializeProperty(feedUse_, "FeedUse")
            serializer.serializePropertyList(addinList_, "AddinList", "Addin")
            serializer.serializeObject(startupActions_, "StartupActions", versionNumber)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' user authentication
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub authenticateUser()

            If userAuthenticated_ Then Exit Sub

            Dim domainList As QuantLibXL.DomainList = Nothing
            Try
                Dim xmlReader As New QuantLibXL.XmlReader(userConfig_, "Configuration")
                xmlReader.serializeObject(domainList, "Domains", THIS_VERSION)
            Catch ex As Exception
                Throw New Exception("Error processing configuration file " & userConfig_ & ":" _
                    & vbCrLf & vbCrLf & ex.Message)
            End Try

            domainList.validate(name_)

            userAuthenticated_ = True

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' validation of paths and files
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub validateFile(ByVal fileName As String, ByVal description As String)

            If Not fileExists(fileName) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & description & ":" & vbCrLf & vbCrLf & "    " _
                    & fileName & vbCrLf & vbCrLf & "could not be found")

            End If

        End Sub

        Private Sub validateDirectory(ByVal directoryName As String, ByVal description As String)

            If Not dirExists(directoryName) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & description & ":" & vbCrLf & vbCrLf & "    " _
                    & directoryName & vbCrLf & vbCrLf & "could not be found")

            End If

        End Sub

        Private Sub validateExcelPath(ByVal excelPath As String)

            If Len(excelPath) > 0 Then
                If Not fileExists(excelPath) Then
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

        Private Sub validate()

            validateDirectory(workbooks_, "workbook directory")
            validateDirectory(helpPath_, "help file directory")
            validateDirectory(xmlPath_, "function metadata directory")

            validateFile(frameworkName_, "VBA framework file")
            validateFile(userConfig_, "user configuration file")

            For Each addin As String In addinList_
                validateFile(addin, "Excel Addin file")
            Next

        End Sub

        ' launch() - Spawn a subprocess running QuantLibXL under Excel.
        ' Write startup parameters to an XML file in a .NET temporary
        ' directory, and set environment variable QUANTLIBXL_LAUNCH to
        ' inform the QuantLibXL session of the location of the file.

        Public Sub launch(ByVal feedAddins() As String, ByVal excelPath As String, ByVal feedUse As String)

            ' Save the original value of addinList_
            Dim keepAddinList() As String = addinList_
            feedUse_ = feedUse

            Try

                ' Temporarily prepend feedAddins (Reuters/Bloomberg) to addinList_
                processFeedAddins(feedAddins)

                validateExcelPath(excelPath)
                validate()
                authenticateUser()

                ' Derive a name for the XML file.  We must guard against race
                ' conditions, i.e. two instances of the Launcher creating two
                ' temp files with the same name.  So the name is in the format
                ' QuantLibXL.launch.xxx.yyy.xml where xxx is this process ID
                ' and yyy is the current time in subseconds.

                Dim tempFilePath As String = System.IO.Path.GetTempPath() _
                    & "QuantLibXL Launcher\"

                System.IO.Directory.CreateDirectory(tempFilePath)

                tempFilePath = tempFilePath _
                    & "QuantLibXL.launch." & GetCurrentProcessId _
                    & "." & DateTime.Now.Ticks & ".xml"

                ' Write this environment object to the temp file

                Dim xmlWriter As New QuantLibXL.XmlWriter(tempFilePath)
                xmlWriter.serializeObject(Me, "Environment", frameworkVersion_)
                xmlWriter.close()

                ' Set the environment variable

                System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH8, tempFilePath)
                System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH9, tempFilePath)
                System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH10, tempFilePath)

                ' Set environment variables specified in the Variables tab

                For Each listItem As ListViewItem In FormMain.lvVariables.Items
                    System.Environment.SetEnvironmentVariable(listItem.Text, listItem.SubItems(1).Text)
                Next

                ' Spawn the subprocess

                Dim commandLine As String
                commandLine = """" & excelPath & """ /e """ & frameworkName_ & """"
                Shell(commandLine, AppWinStyle.NormalFocus)

                ' Restore addinList_ to its original value
                addinList_ = keepAddinList

            Catch ex As Exception

                ' Restore addinList_ to its original value before rethrowing
                addinList_ = keepAddinList
                Throw

            End Try

        End Sub

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = Me.MemberwiseClone()
            Clone.StartupActions = CType(startupActions_, StartupActions).Clone

        End Function

        Public Sub setDotNetParameters()

            Dim addinDir As String
            If (ApplicationDeployment.IsNetworkDeployed) Then
                addinDir = ApplicationDeployment.CurrentDeployment.DataDirectory & "\Addins\" & name_ & "\"
            Else
                addinDir = configPath() & "\Addins\" & name_ & "\"
            End If

            For i As Long = 0 To UBound(addinList_)
                addinList_(i) = addinDir & addinList_(i)
            Next

        End Sub

        ' processFeedAddins() - Temporarily prepend feedAddins (Reuters/Bloomberg) to addinList_.
        ' feedAddins is a transient value and the caller must restore addinList_ to its original state.
        Private Sub processFeedAddins(ByVal feedAddins() As String)

            If feedAddins.Length = 0 Then Exit Sub
            Dim temp(feedAddins.Length + addinList_.Length - 1) As String
            feedAddins.CopyTo(temp, 0)
            addinList_.CopyTo(temp, feedAddins.Length)
            addinList_ = temp

        End Sub

    End Class

End Namespace
