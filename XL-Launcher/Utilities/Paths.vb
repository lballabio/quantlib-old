
Namespace XL_Launcher

    ''' <summary>
    ''' Object to encapsulate the paths.xml file which is delivered by ClickOnce
    ''' and is used to specify the location of the Pre Configured directory.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Paths

        Implements ISerializable

        Private preConfigPath_ As String

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Paths"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = nodeName
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public ReadOnly Property PreConfigPath() As String

            Get
                PreConfigPath = preConfigPath_
            End Get

        End Property

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeProperty(preConfigPath_, "PreConfiguredDirectory")

        End Sub

    End Class

End Namespace
