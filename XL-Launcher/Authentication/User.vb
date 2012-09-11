
Namespace XL_Launcher

    ''' <summary>
    ''' Encapsulate the identity of the user under which the current instance
    ''' of XL-Launcher is running.  Used for authentication.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class User

        Implements ISerializable

        Private name_ As String
        Private machineList_ As Collection = New Collection

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "User"
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

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeObjectCollection(machineList_, "Machine", versionNumber, False)

        End Sub

        Public Function authenticate(ByVal userName As String, ByVal machineName As String) As Boolean

            If name_ = userName Or name_ = "*" Then

                authenticate = 0 = machineList_.Count Or machineList_.Contains(machineName)

            End If

        End Function

    End Class

End Namespace
