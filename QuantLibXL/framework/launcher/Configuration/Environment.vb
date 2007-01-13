
'Copyright (C) 2006 Eric Ehlers

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

        ' startup actions

        Private ycBootstrap_ As Boolean
        Private loadMurexYC_ As Boolean
        Private capVolBootstrap_ As Boolean
        Private swapVolBootstrap_ As Boolean
        Private swapSmileBootstrap_ As Boolean
        Private fitCMS_ As Boolean
        Private indexesTimeSeries_ As Boolean
        Private loadBonds_ As Boolean
        Private staticData_ As Boolean


        Public Sub serialize(ByRef serializer As ISerializer) Implements ISerializable.serialize
            serializer.serializeAttribute(name_, "name")
            serializer.serializeProperty(framework_, "Framework")
            serializer.serializeProperty(workbooks_, "Workbooks")
            serializer.serializeProperty(addinDir_, "AddinDirectory")
            serializer.serializeProperty(addinName_, "AddinName")
            serializer.serializeProperty(helpPath_, "HelpFile")
            serializer.serializeProperty(ycBootstrap_, "YieldCurveBootstrap")
            serializer.serializeProperty(loadMurexYC_, "LoadMurexYieldCurve")
            serializer.serializeProperty(capVolBootstrap_, "CapVolBootstrap")
            serializer.serializeProperty(swapVolBootstrap_, "SwapVolBootstrap")
            serializer.serializeProperty(swapSmileBootstrap_, "SwapSmileBootstrap")
            serializer.serializeProperty(fitCMS_, "FitCMS")
            serializer.serializeProperty(indexesTimeSeries_, "IndexesTimeSeries")
            serializer.serializeProperty(loadBonds_, "LoadBonds")
            serializer.serializeProperty(staticData_, "StaticData")
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

        Public ReadOnly Property CommandLine() As String
            Get
                CommandLine = """" & EXCEL_PATH & """ /e """ & framework_ & """ """ & AddinPath & """"
            End Get
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties - startup actions
        ''''''''''''''''''''''''''''''''''''''''''

        Public Property YieldCurveBootstrap() As Boolean
            Get
                YieldCurveBootstrap = ycBootstrap_
            End Get
            Set(ByVal value As Boolean)
                ycBootstrap_ = value
            End Set
        End Property

        Public Property LoadMurexYieldCurve() As Boolean
            Get
                LoadMurexYieldCurve = loadMurexYC_
            End Get
            Set(ByVal value As Boolean)
                loadMurexYC_ = value
            End Set
        End Property

        Public Property CapVolBootstrap() As Boolean
            Get
                CapVolBootstrap = capVolBootstrap_
            End Get
            Set(ByVal value As Boolean)
                capVolBootstrap_ = value
            End Set
        End Property

        Public Property SwapVolBootstrap() As Boolean
            Get
                SwapVolBootstrap = swapVolBootstrap_
            End Get
            Set(ByVal value As Boolean)
                swapVolBootstrap_ = value
            End Set
        End Property

        Public Property SwapSmileBootstrap() As Boolean
            Get
                SwapSmileBootstrap = swapSmileBootstrap_
            End Get
            Set(ByVal value As Boolean)
                swapSmileBootstrap_ = value
            End Set
        End Property

        Public Property FitCMS() As Boolean
            Get
                FitCMS = fitCMS_
            End Get
            Set(ByVal value As Boolean)
                fitCMS_ = value
            End Set
        End Property

        Public Property IndexesTimeSeries() As Boolean
            Get
                IndexesTimeSeries = indexesTimeSeries_
            End Get
            Set(ByVal value As Boolean)
                indexesTimeSeries_ = value
            End Set
        End Property

        Public Property LoadBonds() As Boolean
            Get
                LoadBonds = loadBonds_
            End Get
            Set(ByVal value As Boolean)
                loadBonds_ = value
            End Set
        End Property
        Public Property StaticData() As Boolean
            Get
                StaticData = staticData_
            End Get
            Set(ByVal value As Boolean)
                staticData_ = value
            End Set
        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' subroutines
        ''''''''''''''''''''''''''''''''''''''''''

        Private Sub validate()

            If Not fileExists(framework_) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & "VBA framework file:" & vbCrLf & vbCrLf & "    " _
                    & framework_ & vbCrLf & vbCrLf & "could not be found")
            End If

            If Not dirExists(workbooks_) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & "workbook directory:" & vbCrLf & vbCrLf & "    " _
                    & workbooks_ & vbCrLf & vbCrLf & "could not be found")
            End If

            If Not fileExists(AddinPath) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & "XLL addin file:" & vbCrLf & vbCrLf & "    " _
                    & AddinPath & vbCrLf & vbCrLf & "could not be found")
            End If

            If Not dirExists(helpPath_) Then
                Throw New Exception("Environment """ & name_ & """ - " _
                    & "help file directory:" & vbCrLf & vbCrLf & "    " _
                    & helpPath_ & vbCrLf & vbCrLf & "could not be found")
            End If

        End Sub

        Public Sub launch()

            validate()

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
            copy.ycBootstrap_ = ycBootstrap_
            copy.loadMurexYC_ = loadMurexYC_
            copy.capVolBootstrap_ = capVolBootstrap_
            copy.swapVolBootstrap_ = swapVolBootstrap_
            copy.swapSmileBootstrap_ = swapSmileBootstrap_
            copy.fitCMS_ = fitCMS_
            copy.indexesTimeSeries_ = indexesTimeSeries_
            copy.loadBonds_ = loadBonds_
            copy.staticData_ = staticData_
        End Function

        Public Sub overrideActions(ByVal envOverride As Environment)
            ' override the startup actions in this environment
            ' with those of the indicated environment
            ycBootstrap_ = envOverride.ycBootstrap_
            loadMurexYC_ = envOverride.loadMurexYC_
            capVolBootstrap_ = envOverride.capVolBootstrap_
            swapVolBootstrap_ = envOverride.swapVolBootstrap_
            swapSmileBootstrap_ = envOverride.swapSmileBootstrap_
            fitCMS_ = envOverride.fitCMS_
            indexesTimeSeries_ = envOverride.indexesTimeSeries_
            loadBonds_ = envOverride.loadBonds_
            staticData_ = envOverride.staticData_
        End Sub

        Public Sub setConfigured()
            If (ApplicationDeployment.IsNetworkDeployed) Then
                addinDir_ = ApplicationDeployment.CurrentDeployment.DataDirectory & "\Addins\" & name_
            Else
                addinDir_ = configPath() & "\Addins\" & name_
            End If
        End Sub

    End Class

End Namespace
