
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

    Public Class Configuration
        Implements ISerializable

        Public Const REUTERS_PATH_DEFAULT As String = "C:\Program Files\Reuters\PowerPlus"
        Public Const BLOOMBERG_PATH_DEFAULT As String = "C:\blp\API\dde"
        Public Const REUTERS_XLA_DEFAULT As String = "PPP.xla"
        Public Const BLOOMBERG_XLA_DEFAULT As String = "BlpMain.xla"

        Private selectedEnvConfig_ As String = ""
        Private selectedEnvName_ As String = ""
        Private reutersPath_ As String = ""
        Private bloombergPath_ As String = ""
        Private reutersSelected_ As Boolean = False
        Private bloombergSelected_ As Boolean = False
        Private reutersEnabled_ As Boolean = False
        Private bloombergEnabled_ As Boolean = False
        Private excelPath_ As String = ""
        Private feedUse_ As String = ""
        Private overrideActions_ As QuantLibXL.StartupActionsList

        Public Property Name() As String Implements ISerializable.Name
            Get
                Name = "Configuration"
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Property SelectedEnvConfig() As String

            Get
                Return selectedEnvConfig_
            End Get

            Set(ByVal value As String)
                selectedEnvConfig_ = value
            End Set

        End Property

        Public Property SelectedEnvName() As String

            Get
                Return selectedEnvName_
            End Get

            Set(ByVal value As String)
                selectedEnvName_ = value
            End Set

        End Property

        Public Property OverrideActions() As QuantLibXL.StartupActionsList

            Get
                Return overrideActions_
            End Get

            Set(ByVal value As QuantLibXL.StartupActionsList)
                overrideActions_ = value
            End Set

        End Property

        Public Property ReutersPath() As String

            Get
                Return reutersPath_
            End Get

            Set(ByVal value As String)
                reutersPath_ = value
            End Set

        End Property

        Public Property BloombergPath() As String

            Get
                Return bloombergPath_
            End Get

            Set(ByVal value As String)
                bloombergPath_ = value
            End Set

        End Property

        Public Property ReutersSelected() As Boolean

            Get
                Return reutersSelected_
            End Get

            Set(ByVal value As Boolean)
                reutersSelected_ = value
            End Set

        End Property

        Public Property BloombergSelected() As Boolean

            Get
                Return bloombergSelected_
            End Get

            Set(ByVal value As Boolean)
                bloombergSelected_ = value
            End Set

        End Property

        Public Property ReutersEnabled() As Boolean

            Get
                Return reutersEnabled_
            End Get

            Set(ByVal value As Boolean)
                reutersEnabled_ = value
            End Set

        End Property

        Public Property BloombergEnabled() As Boolean

            Get
                Return bloombergEnabled_
            End Get

            Set(ByVal value As Boolean)
                bloombergEnabled_ = value
            End Set

        End Property

        Public Property ExcelPath() As String

            Get
                Return excelPath_
            End Get

            Set(ByVal value As String)
                excelPath_ = value
            End Set

        End Property

        Public Property FeedUse() As String

            Get
                Return feedUse_
            End Get

            Set(ByVal value As String)
                feedUse_ = value
            End Set

        End Property

        ' Generate a list of feed addins to be loaded by the Framework
        Public ReadOnly Property FeedList() As String()

            Get
                Dim usingReuters As Boolean = reutersSelected_ And reutersEnabled_
                Dim usingBloomberg As Boolean = bloombergSelected_ And bloombergEnabled_

                Dim ret(boolToInt(usingReuters) + boolToInt(usingBloomberg) - 1) As String
                Dim idx As Integer = 0
                If usingReuters Then
                    ret(0) = reutersPath_
                    idx = 1
                End If
                If usingBloomberg Then
                    ret(idx) = bloombergPath_
                End If

                FeedList = ret
            End Get

        End Property

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeProperty(selectedEnvConfig_, "SelectedEnvConfig")
            serializer.serializeProperty(selectedEnvName_, "SelectedEnvName")
            serializer.serializeProperty(reutersPath_, "ReutersPath")
            serializer.serializeProperty(bloombergPath_, "BloombergPath")
            serializer.serializeProperty(reutersSelected_, "ReutersSelected")
            serializer.serializeProperty(bloombergSelected_, "BloombergSelected")
            serializer.serializeProperty(excelPath_, "ExcelPath")
            serializer.serializeProperty(feedUse_, "FeedUse")
            serializer.serializeObject(overrideActions_, "StartupActionsList", versionNumber)

        End Sub

        Public Sub serialize2(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize2
        End Sub

        Private Function boolToInt(ByVal b As Boolean) As Integer
            If b Then boolToInt = 1 Else boolToInt = 0
        End Function

    End Class

End Namespace
