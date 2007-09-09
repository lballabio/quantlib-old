
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

Partial Public Class FormMain

    Private Sub enableVariableMenu()

        If lvVariables.SelectedIndices.Count = 0 Then
            miVariableDelete.Enabled = False
            miVariableEdit.Enabled = False
        Else
            miVariableDelete.Enabled = True
            miVariableEdit.Enabled = True
        End If

    End Sub

    Private Sub initializeVariableList()

        lvVariables.Items.Clear()
        For Each variable As QuantLibXL.Variable In selectedEnvironment_.VariableList.Variables
            Dim listItem As New ListViewItem(variable.Name)
            listItem.SubItems.Add(variable.Value)
            lvVariables.Items.Add(listItem)
        Next

    End Sub

End Class
