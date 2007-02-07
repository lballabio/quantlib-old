
'Copyright (C) 2006, 2007 Eric Ehlers

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

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Class Environment - Encapsulate the state and behavior relating to
' a running instance of the QuantLibXL Framework.
'
' This class contains some temporary hacks to support backward compatibility
' with Framework version 5, which relies on an older version of the 
' Launcher<->Framework interface.
'
' Version information is included in version 6+ of the Launcher<->Framework 
' interface and the hacks can be removed once version 5 of the Framewok
' is retired.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Imports System.Deployment.Application

Namespace QuantLibXL

    Public Class Environment
        Implements ICloneable
        Implements ISerializable

        Private Declare Function GetCurrentProcessId Lib "kernel32" () As Long

        ' Environment variable to inform the Framework of the location
        ' of the configuration file.
        ' QUANTLIBXL_LAUNCH is for Framework version 5.
        ' QUANTLIBXL_LAUNCH2 is for Framework version 6+.

        Private Const QUANTLIBXL_LAUNCH As String = "QUANTLIBXL_LAUNCH"
        Private Const QUANTLIBXL_LAUNCH2 As String = "QUANTLIBXL_LAUNCH2"

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
        Private addinList_(0) As String
        Private helpPath_ As String = ""
        Private xmlPath_ As String = ""
        Private userConfig_ As String = ""

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

        Public ReadOnly Property CommandLine() As String

            Get
                CommandLine = """" & EXCEL_PATH & """ /e """ & frameworkName_ & """"
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        ' serialize() - Write this object to the given serializer.
        ' Version 6 supports a list of addin names.
        ' Version 5 requires exactly one addin name, split into directory and file.

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeProperty(frameworkName_, "Framework")
            serializer.serializeProperty(frameworkVersion_, "FrameworkVersion")
            serializer.serializeProperty(workbooks_, "Workbooks")
            serializer.serializeProperty(helpPath_, "HelpFile")
            serializer.serializeProperty(xmlPath_, "FunctionMetadata")
            serializer.serializeProperty(userConfig_, "UserConfigurationFile")

            If versionNumber = 5 Then

                If addinList_.Length <> 1 Then

                    Throw New Exception("Unable to process addin list for Framework version 5." _
                        & " The addin list contains " & addinList_.Length & " items" _
                        & " but version 5 of the Framework expects exactly one addin.")

                End If

                Dim addinFullName As String = addinList_(0)
                Dim addinDirectory As String = System.IO.Path.GetDirectoryName(addinFullName)
                Dim addinFile As String = System.IO.Path.GetFileName(addinFullName)

                serializer.serializeProperty(addinDirectory, "AddinDirectory")
                serializer.serializeProperty(addinFile, "AddinName")

            Else

                serializer.serializePropertyList(addinList_, "AddinList", "Addin")

            End If

            serializer.serializeObject(startupActions_, "StartupActions", versionNumber)

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties - startup actions
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property StartupActions() As QuantLibXL.StartupActions

            Get
                StartupActions = startupActions_
            End Get

            Set(ByVal value As QuantLibXL.StartupActions)
                startupActions_ = value
            End Set

        End Property

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

        Private Sub validate()

            validateDirectory(workbooks_, "workbook directory")
            validateDirectory(helpPath_, "help file directory")
            validateDirectory(xmlPath_, "function metadata directory")

            validateFile(frameworkName_, "VBA framework file")
            validateFile(userConfig_, "user configuration file")

            For Each addin As String In addinList_
                validateFile(addin, "XLL addin file")
            Next

        End Sub

        ' launch() - Spawn a subprocess running QuantLibXL under Excel.
        ' Write startup parameters to an XML file in a .NET temporary
        ' directory, and set environment variable QUANTLIBXL_LAUNCH to
        ' inform the QuantLibXL session of the location of the file.

        Public Sub launch()

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

            ' Write this environment object to the temp file.

            Dim xmlWriter As New QuantLibXL.XmlWriter(tempFilePath)
            xmlWriter.serializeObject(Me, "Environment", frameworkVersion_)
            xmlWriter.close()

            ' Set the environment variable.

            System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH, tempFilePath)
            System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH2, tempFilePath)

            ' Spawn the subprocess.

            Shell(CommandLine, AppWinStyle.NormalFocus)

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

    End Class

End Namespace
