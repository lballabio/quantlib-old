
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

Namespace QuantLibXL

    Public Class StartupActions
        Implements ICloneable
        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String = "StartupActions"

        Private setEvaluationDate_ As Boolean
        Private evaluationDateValue_ As Integer
        Private ycBootstrap_ As Boolean
        Private capVolBootstrap_ As Boolean
        Private swapSmileBootstrap_ As Boolean
        Private calibrateCMS_ As Boolean
        Private fitCMS_ As Boolean
        Private indexesTimeSeries_ As Boolean
        Private loadBonds_ As Boolean
        Private mainChecks_ As Boolean
        Private staticData_ As Boolean
        Private initSource_ As String = "Excel"

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeProperty(setEvaluationDate_, "SetEvaluationDate")
            serializer.serializeProperty(evaluationDateValue_, "EvaluationDate")
            serializer.serializeProperty(ycBootstrap_, "YieldCurveBootstrap")
            serializer.serializeProperty(capVolBootstrap_, "CapVolBootstrap")
            serializer.serializeProperty(swapSmileBootstrap_, "SwapSmileBootstrap")
            serializer.serializeProperty(calibrateCMS_, "CalibrateCMS")
            serializer.serializeProperty(fitCMS_, "FitCMS")
            serializer.serializeProperty(indexesTimeSeries_, "IndexesTimeSeries")
            serializer.serializeProperty(loadBonds_, "LoadBonds")
            serializer.serializeProperty(mainChecks_, "MainChecks")
            serializer.serializeProperty(staticData_, "StaticData")
            serializer.serializeProperty(initSource_, "InitSource")
            If versionNumber < 10 Then
                serializer.serializeProperty(False, "LoadMurexYieldCurve")
            End If

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
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
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        ' Internally, the evaluation date is stored in integer evaluationDateValue_
        ' which stores the value in Excel's "serial number" format.
        ' Externally the evaluation date is represented by the VB .NET Date type.
        ' The Get/Set methods of the property below perform the conversions.

        Public Property EvaluationDate() As Date

            Get
                If evaluationDateValue_ = 0 Then
                    EvaluationDate = System.DateTime.Today
                Else
                    EvaluationDate = New DateTime((evaluationDateValue_ + 693593) * (10000000L * 24 * 3600))
                End If
            End Get

            Set(ByVal d As Date)
                evaluationDateValue_ = CInt((d.Ticks / (10000000L * 24 * 3600)) - 693593)
            End Set

        End Property

        Public Property SetEvaluationDate() As Boolean

            Get
                SetEvaluationDate = setEvaluationDate_
            End Get

            Set(ByVal value As Boolean)
                setEvaluationDate_ = value
            End Set

        End Property

        Public Property YieldCurveBootstrap() As Boolean

            Get
                YieldCurveBootstrap = ycBootstrap_
            End Get

            Set(ByVal value As Boolean)
                ycBootstrap_ = value
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

        Public Property SwapSmileBootstrap() As Boolean

            Get
                SwapSmileBootstrap = swapSmileBootstrap_
            End Get

            Set(ByVal value As Boolean)
                swapSmileBootstrap_ = value
            End Set

        End Property

        Public Property CalibrateCMS() As Boolean

            Get
                CalibrateCMS = calibrateCMS_
            End Get

            Set(ByVal value As Boolean)
                calibrateCMS_ = value
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

        Public Property MainChecks() As Boolean

            Get
                MainChecks = mainChecks_
            End Get

            Set(ByVal value As Boolean)
                mainChecks_ = value
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

        Public Property InitSource() As String

            Get
                InitSource = initSource_
            End Get

            Set(ByVal value As String)
                initSource_ = value
            End Set

        End Property

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = Me.MemberwiseClone()

        End Function

    End Class

End Namespace

