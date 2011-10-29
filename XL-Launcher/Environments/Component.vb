
Namespace XL_Launcher

    ''' <summary>
    ''' An Addin or other object to be loaded into Excel.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Component


        Implements ICloneable
        Implements ISerializable

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private path_ As String

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Component"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = path_
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Property path() As String

            Get
                Return path_
            End Get

            Set(ByVal value As String)
                path_ = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = Me.MemberwiseClone()

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(path_, "path")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal path As String)

            path_ = path

        End Sub

        Public Sub New()
        End Sub

    End Class

End Namespace
