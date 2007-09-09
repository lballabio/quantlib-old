
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

    Private Sub setReutersPathEnabled()
        Dim reutersPathValid = fileExists(txtReuters.Text)
        cbReuters.Enabled = reutersPathValid
        config_.ReutersEnabled = reutersPathValid
    End Sub

    Private Sub setBloombergPathEnabled()
        Dim bloombergPathValid = fileExists(txtBloomberg.Text)
        cbBloomberg.Enabled = bloombergPathValid
        config_.BloombergEnabled = bloombergPathValid
    End Sub

    Private Sub setFeedUse()
        If rbReuters.Checked Then
            config_.FeedUse = "Reuters"
        ElseIf rbBloomberg.Checked Then
            config_.FeedUse = "Bloomberg"
        Else
            Throw New Exception("Error specifying Reuters/Bloomberg.")
        End If
    End Sub

End Class
