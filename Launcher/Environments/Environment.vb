
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

Imports System.Deployment.Application

Namespace QuantLibXL

    Public Class Environment
        Implements ISerializable

        Private Declare Function GetCurrentProcessId Lib "kernel32" () As Long

        Private Const QUANTLIBXL_LAUNCH As String = "QUANTLIBXL_LAUNCH"

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String

        ' paths

        Private framework_ As String
        Private workbooks_ As String
        Private addinDir_ As String
        Private addinName_ As String
        Private helpPath_ As String
        Private xmlPath_ As String
        Private userConfig_ As String

        ' user authentication

        Private userAuthenticated_ As Boolean = False

        ' startup actions

        Private startupActions_ As QuantLibXL.StartupActions = Nothing

        Public Sub serialize(ByRef serializer As ISerializer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeProperty(framework_, "Framework")
            serializer.serializeProperty(workbooks_, "Workbooks")
            serializer.serializeProperty(addinDir_, "AddinDirectory")
            serializer.serializeProperty(addinName_, "AddinName")
            serializer.serializeProperty(helpPath_, "HelpFile")
            serializer.serializeProperty(xmlPath_, "FunctionMetadata")
            serializer.serializeProperty(userConfig_, "UserConfigurationFile")
            serializer.serializeObject(startupActions_, "StartupActions")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = name_
            End Get

            Set(ByVal value As String)
                name_ = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties - paths
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property Framework() As String
            Get
                Framework = framework_
            End Get
            Set(ByVal value As String)
                framework_ = value
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

        Public Property AddinDirectory() As String
            Get
                AddinDirectory = addinDir_
            End Get
            Set(ByVal value As String)
                addinDir_ = value
            End Set
        End Property

        Public Property AddinName() As String
            Get
                AddinName = addinName_
            End Get
            Set(ByVal value As String)
                addinName_ = value
            End Set
        End Property

        Private ReadOnly Property AddinPath() As String
            Get
                AddinPath = addinDir_ & "\" & addinName_
            End Get
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
                CommandLine = """" & EXCEL_PATH & """ /e """ & framework_ & """ """ & AddinPath & """"
            End Get
        End Property

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
                xmlReader.serializeObject(domainList, "Domains")
            Catch ex As Exception
                Throw New Exception("Error processing configuration file " & userConfig_ & ":" _
                    & vbCrLf & vbCrLf & ex.Message)
            End Try

            domainList.validate()

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

            validateFile(framework_, "VBA framework file")
            validateFile(AddinPath, "XLL addin file")
            validateFile(userConfig_, "user configuration file")

        End Sub

        Public Sub launch()

            validate()
            authenticateUser()

            Dim tempFilePath As String = System.IO.Path.GetTempPath() _
                & "QuantLibXL.launch." & GetCurrentProcessId _
                & "." & DateTime.Now.Ticks & ".xml"
            Dim xmlWriter As New QuantLibXL.XmlWriter(tempFilePath)
            xmlWriter.serializeObject(Me, "Environment")
            xmlWriter.close()

            System.Environment.SetEnvironmentVariable(QUANTLIBXL_LAUNCH, tempFilePath)
            Shell(CommandLine, AppWinStyle.NormalFocus)
        End Sub

        Public Function copy(ByVal copyName As String) As Environment

            copy = New Environment
            copy.name_ = copyName
            copy.framework_ = framework_
            copy.workbooks_ = workbooks_
            copy.addinDir_ = addinDir_
            copy.addinName_ = addinName_
            copy.helpPath_ = helpPath_
            copy.xmlPath_ = xmlPath_
            copy.userConfig_ = userConfig_
            copy.startupActions_ = startupActions_

        End Function

        Public Sub setConfigured()

            If (ApplicationDeployment.IsNetworkDeployed) Then
                addinDir_ = ApplicationDeployment.CurrentDeployment.DataDirectory & "\Addins\" & name_
            Else
                addinDir_ = configPath() & "\Addins\" & name_
            End If

        End Sub

    End Class

End Namespace
