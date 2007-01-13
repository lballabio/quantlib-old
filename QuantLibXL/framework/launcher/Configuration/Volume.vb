
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

' This module implements function getSerialNumber() which returns the
' serial number of the volume as a hexadecimal string.  
' - The function does not specify the name of the volume to be queried,
'   so the serial number returned is that of the active volume.
' - The serial number returned is that of the volume, not of the physical
'   hard drive.  The volume serial number changes when the hard drive
'   is reformatted.

Imports System.Runtime.InteropServices

Module Volume

    <DllImportAttribute("kernel32.dll")> _
    Private Function GetVolumeInformation( _
            ByVal PathName As String, _
            ByVal VolumeNameBuffer As String, _
            ByVal VolumeNameSize As UInt32, _
            ByRef VolumeSerialNumber As UInt32, _
            ByRef MaximumComponentLength As UInt32, _
            ByRef FileSystemFlags As UInt32, _
            ByVal FileSystemNameBuffer As String, _
            ByVal FileSystemNameSize As UInt32) As Boolean
    End Function

    Public Function getSerialNumber() As String
        Dim serialNumber As UInt32
        Dim returnCode As Boolean = GetVolumeInformation( _
            vbNullString, vbNullString, 0, _
            serialNumber, 0, 0, vbNullString, 0)
        If Not returnCode Or serialNumber = 0 Then
            Throw New Exception("unable to retrieve hard drive serial number")
        End If
        getSerialNumber = Hex(serialNumber)
    End Function

End Module
