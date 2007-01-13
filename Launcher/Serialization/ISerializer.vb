
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

Namespace QuantLibXL

    Public Interface ISerializer

        Sub serializeObject(ByRef serializable As ISerializable, ByVal className As String)
        Sub serializeObjectCollection(ByRef serializableList As Collection, ByVal className As String)
        Sub serializeAttribute(ByRef attr As String, ByVal tag As String)
        Overloads Sub serializeProperty(ByRef prop As String, ByVal tag As String)
        Overloads Sub serializeProperty(ByRef prop As String(), ByVal tag As String)
        Overloads Sub serializeProperty(ByRef prop As Boolean, ByVal tag As String)
        Overloads Sub serializeProperty(ByRef prop As Integer, ByVal tag As String)
        Sub close()

    End Interface

End Namespace