
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

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Addins
    ''''''''''''''''''''''''''''''''''''''''''

    ' Sub enableAddinButtons() - Based on the current selection,
    ' enable or disable the following buttons: Delete, Rename, Up, Down

    Private Sub enableAddinButtons()

        If lbAddins.SelectedIndex = -1 Then
            ' Nothing selected - disable all buttons
            btnAddinDelete.Enabled = False
            btnAddinRename.Enabled = False
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            Exit Sub
        End If

        ' Something is selected, so enable Delete/Rename
        btnAddinDelete.Enabled = True
        btnAddinRename.Enabled = True

        If lbAddins.Items.Count <= 1 Then
            ' Only one item in list - disable Up/Down
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            Exit Sub
        End If

        If lbAddins.SelectedIndex = 0 Then
            ' Top item selected - disable Up, enable Down
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = True
        ElseIf lbAddins.SelectedIndex = (lbAddins.Items.Count - 1) Then
            ' Bottom item selected - enable Up, disable Down
            btnAddinUp.Enabled = True
            btnAddinDown.Enabled = False
        Else
            ' Middle item selected - enable Up & Down
            btnAddinUp.Enabled = True
            btnAddinDown.Enabled = True
        End If

    End Sub

    Private Sub initializeAddinList()

        lbAddins.Items.Clear()
        For Each addin As QuantLibXL.Addin In SelectedEnvironment.AddinList.Addins
            lbAddins.Items.Add(addin.Path)
        Next

        enableAddinButtons()

    End Sub

End Class
