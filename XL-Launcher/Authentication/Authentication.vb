
Imports System

Namespace XL_Launcher

    ''' <summary>
    ''' Encapsulate the functionality to serialize a stream of authentication data
    ''' and the logic to determine whether the current user satisfies the given
    ''' criteria for accessing the corresponding Environment.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Authentication

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' members
        ''''''''''''''''''''''''''''''''''''''''''

        Private domainList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Authentication"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = nodeName
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection(domainList_, "Domain", versionNumber, False)

        End Sub

        Public Function authenticate() As Boolean

            If 0 = domainList_.Count Then

                authenticate = True
                Exit Function

            End If

            Dim ap As AuthenticationParameters = deriveAuthenticationParameters()

            For Each d As Domain In domainList_

                If d.authenticate(ap.domainName, ap.userName, ap.machineName) Then

                    authenticate = True
                    Exit Function

                End If

            Next d

        End Function

    End Class

End Namespace
