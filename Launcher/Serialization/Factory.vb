
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

    Public Class Factory

        Public Shared Function make(ByVal className As String) As Object

            Select Case className
                Case "Addin"
                    make = New Addin()
                Case "AddinList"
                    make = New AddinList()
                Case "Configuration"
                    make = New Configuration()
                Case "Domain"
                    make = New Domain()
                Case "Domains"
                    make = New DomainList()
                Case "Environment"
                    make = New Environment()
                Case "Environments"
                    make = New EnvironmentList()
                Case "StartupActions"
                    make = New StartupActions()
                Case "StartupActionsList"
                    make = New StartupActionsList()
                Case "User"
                    make = New User()
                Case "Users"
                    make = New UserList()
                Case "Variable"
                    make = New Variable()
                Case "Variables"
                    make = New VariableList()
                Case Else
                    Throw New Exception("invalid class name: " & className)
            End Select

        End Function

    End Class

End Namespace
