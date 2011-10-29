
Namespace XL_Launcher

    ''' <summary>
    ''' A collection of utilities relating to Serialization.
    ''' </summary>
    ''' <remarks></remarks>
    Module Serialization

        Public Function loadObject(ByVal xmlFile As String, ByVal nodeName As String) As Object

            loadObject = Nothing

            Try

                Dim xmlReader2 As New XmlReader(xmlFile)
                xmlReader2.serializeObject(loadObject, nodeName, THIS_VERSION)

            Catch ex As Exception

                Throw New Exception("Error loading file" & vbCrLf & vbCrLf & _
                    xmlFile & vbCrLf & vbCrLf & ex.Message)

            End Try

        End Function

        Public Function loadObject(ByVal xmlFile As String, ByVal rootNode As String, _
            ByVal nodeName As String) As Object

            loadObject = Nothing

            Try

                Dim xmlReader2 As New XmlReader(xmlFile, rootNode)
                xmlReader2.serializeObject(loadObject, nodeName, THIS_VERSION)

            Catch ex As Exception

                Throw New Exception("Error loading file" & vbCrLf & vbCrLf & _
                    xmlFile & vbCrLf & vbCrLf & ex.Message)

            End Try

        End Function

        Public Sub saveObject(ByVal xmlFile As String, ByVal o As Object, ByVal nodeName As String)

            Try

                Dim xmlWriter2 As New XmlWriter(xmlFile)
                xmlWriter2.serializeObject(o, nodeName, THIS_VERSION)
                xmlWriter2.close()

            Catch ex As Exception

                Throw New Exception("Error writing file" & vbCrLf & vbCrLf & _
                    xmlFile & vbCrLf & vbCrLf & ex.Message)

            End Try

        End Sub

    End Module

End Namespace
