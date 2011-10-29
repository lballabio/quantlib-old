
Namespace XL_Launcher

    ''' <summary>
    ''' Encapsulate the information relating to the domain
    ''' to which the current user belongs.  Used for authentication.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Domain

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private userList_ As Collection = New Collection

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Domain"
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
            serializer.serializeObjectCollection(userList_, "User", versionNumber, False)

        End Sub

        Public Function authenticate(ByVal domainName As String, ByVal userName As String, ByVal machineName As String) As Boolean

            If name_ = domainName Or name_ = "*" Then

                If 0 = userList_.Count Then

                    authenticate = True

                Else

                    For Each u As User In userList_

                        If u.authenticate(userName, machineName) Then

                            authenticate = True
                            Exit Function

                        End If

                    Next u

                End If

            End If

        End Function

    End Class

End Namespace
