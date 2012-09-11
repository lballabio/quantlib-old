
Namespace XL_Launcher

    ''' <summary>
    ''' A Key-Value pair.  The StartupParameter encapsulates some kind of control
    ''' (e.g. TextBox, DatePicker) which is displayed in the UI in order to prompt
    ''' the user for the corresponding data.  Once collected the data is transmitted
    ''' to Excel in the session file to be consumed by Launcher.xla.
    ''' </summary>
    ''' <remarks></remarks>
    Public Interface StartupParameter

        Inherits ICloneable

        ReadOnly Property parameterName() As String
        ReadOnly Property typeName() As String
        Property displayValue() As Object
        ReadOnly Property transmitValue() As Object
        Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer)
        ReadOnly Property height() As Integer
        ReadOnly Property visible() As Boolean

    End Interface

    Public Class KeyValuePair

        Implements ICloneable
        Implements ISerializable
        Implements StartupParameter

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private value_ As String

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "KeyValuePair"
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

        Public ReadOnly Property parameterName() As String Implements StartupParameter.parameterName

            Get
                parameterName = name_
            End Get

        End Property

        Public ReadOnly Property typeName() As String Implements StartupParameter.typeName

            Get
                typeName = nodeName
            End Get

        End Property

        Public Property displayValue() As Object Implements StartupParameter.displayValue

            Get
                displayValue = value_
            End Get

            Set(ByVal v As Object)
                value_ = v
            End Set

        End Property

        Public ReadOnly Property transmitValue() As Object Implements StartupParameter.transmitValue

            Get
                transmitValue = displayValue
            End Get

        End Property

        Public ReadOnly Property height() As Integer Implements StartupParameter.height

            Get
                height = 0
            End Get

        End Property

        Public ReadOnly Property visible() As Boolean Implements StartupParameter.visible

            Get
                visible = False
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

        End Sub

        Public Sub New(ByVal name As String, ByVal value As String)

            name_ = name
            value_ = value

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New KeyValuePair(name_, value_)

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeAttribute(value_, "value", "")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' StartupParameter Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer) Implements StartupParameter.draw

        End Sub

    End Class

    Public Class CheckBox

        Implements ICloneable
        Implements ISerializable
        Implements StartupParameter

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private checkBox_ As System.Windows.Forms.CheckBox

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "CheckBox"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = checkBox_.Text
            End Get

            Set(ByVal value As String)
                checkBox_.Text = value
            End Set

        End Property

        Public ReadOnly Property parameterName() As String Implements StartupParameter.parameterName

            Get
                parameterName = checkBox_.Text
            End Get

        End Property

        Public ReadOnly Property typeName() As String Implements StartupParameter.typeName

            Get
                typeName = nodeName
            End Get

        End Property

        Public Property displayValue() As Object Implements StartupParameter.displayValue

            Get
                displayValue = checkBox_.Checked
            End Get

            Set(ByVal v As Object)
                checkBox_.Checked = v
            End Set

        End Property

        Public ReadOnly Property transmitValue() As Object Implements StartupParameter.transmitValue

            Get
                transmitValue = displayValue
            End Get

        End Property

        Public ReadOnly Property height() As Integer Implements StartupParameter.height

            Get
                height = 25
            End Get

        End Property

        Public ReadOnly Property visible() As Boolean Implements StartupParameter.visible

            Get
                visible = True
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

            checkBox_ = New System.Windows.Forms.CheckBox()
            checkBox_.Width = 300

        End Sub

        Public Sub New(ByVal text As String, ByVal checked As Boolean, ByVal enabled As Boolean)

            checkBox_ = New System.Windows.Forms.CheckBox()
            checkBox_.Width = 300
            checkBox_.Text = text
            checkBox_.Checked = checked
            checkBox_.Enabled = enabled

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New CheckBox(checkBox_.Text, checkBox_.Checked, checkBox_.Enabled)

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(checkBox_.Text, "text")
            serializer.serializeAttribute(checkBox_.Checked, "checked", "False")
            serializer.serializeAttribute(checkBox_.Enabled, "enabled", "True")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' StartupParameter Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer) Implements StartupParameter.draw

            checkBox_.Location = New Point(10, y)
            c.Add(checkBox_)

        End Sub

    End Class

    Public Class TextBox

        Implements ICloneable
        Implements ISerializable
        Implements StartupParameter

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private textBox_ As System.Windows.Forms.TextBox
        Private label_ As System.Windows.Forms.Label

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "TextBox"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = label_.Text
            End Get

            Set(ByVal value As String)
                label_.Text = value
            End Set

        End Property

        Public ReadOnly Property parameterName() As String Implements StartupParameter.parameterName

            Get
                parameterName = label_.Text
            End Get

        End Property

        Public ReadOnly Property typeName() As String Implements StartupParameter.typeName

            Get
                typeName = nodeName
            End Get

        End Property

        Public Property displayValue() As Object Implements StartupParameter.displayValue

            Get
                displayValue = textBox_.Text
            End Get

            Set(ByVal v As Object)
                textBox_.Text = v
            End Set

        End Property

        Public ReadOnly Property transmitValue() As Object Implements StartupParameter.transmitValue

            Get
                transmitValue = displayValue
            End Get

        End Property

        Public ReadOnly Property height() As Integer Implements StartupParameter.height

            Get
                height = 25
            End Get

        End Property

        Public ReadOnly Property visible() As Boolean Implements StartupParameter.visible

            Get
                visible = True
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

            textBox_ = New System.Windows.Forms.TextBox
            textBox_.Width = 275
            label_ = New System.Windows.Forms.Label
            label_.Width = 140

        End Sub

        Public Sub New(ByVal label As String, ByVal text As String, ByVal enabled As Boolean)

            textBox_ = New System.Windows.Forms.TextBox
            textBox_.Width = 275
            textBox_.Text = text
            textBox_.Enabled = enabled
            label_ = New System.Windows.Forms.Label
            label_.Width = 140
            label_.Text = label

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New TextBox(label_.Text, textBox_.Text, textBox_.Enabled)

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(label_.Text, "label")
            serializer.serializeAttribute(textBox_.Text, "text", "")
            serializer.serializeAttribute(textBox_.Enabled, "enabled", "True")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' StartupParameter Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer) Implements StartupParameter.draw

            label_.Location = New Point(10, y)
            label_.Enabled = textBox_.Enabled
            c.Add(label_)

            textBox_.Location = New Point(150, y)
            c.Add(textBox_)

        End Sub

    End Class

    Public Class ComboBox

        Implements ICloneable
        Implements ISerializable
        Implements StartupParameter

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private selectedItem_ As String
        Private items_() As String
        Private label_ As System.Windows.Forms.Label
        Private WithEvents comboBox_ As System.Windows.Forms.ComboBox

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "ComboBox"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = label_.Text
            End Get

            Set(ByVal value As String)
                label_.Text = value
            End Set

        End Property

        Public ReadOnly Property parameterName() As String Implements StartupParameter.parameterName

            Get
                parameterName = label_.Text
            End Get

        End Property

        Public ReadOnly Property typeName() As String Implements StartupParameter.typeName

            Get
                typeName = nodeName
            End Get

        End Property

        Public Property displayValue() As Object Implements StartupParameter.displayValue

            Get
                displayValue = selectedItem_
            End Get

            Set(ByVal v As Object)
                selectedItem_ = v
            End Set

        End Property

        Public ReadOnly Property transmitValue() As Object Implements StartupParameter.transmitValue

            Get
                transmitValue = comboBox_.Text
            End Get

        End Property

        Public ReadOnly Property height() As Integer Implements StartupParameter.height

            Get
                height = 25
            End Get

        End Property

        Public ReadOnly Property visible() As Boolean Implements StartupParameter.visible

            Get
                visible = True
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

            label_ = New System.Windows.Forms.Label
            label_.Width = 140

            comboBox_ = New System.Windows.Forms.ComboBox
            comboBox_.Width = 150
            comboBox_.DropDownStyle = ComboBoxStyle.DropDownList

        End Sub

        Public Sub New(ByVal label As String, ByVal selectedItem As String, ByVal transmitValue As String, _
            ByVal enabled As Boolean, ByVal items() As String)

            label_ = New System.Windows.Forms.Label
            label_.Width = 140
            label_.Text = label

            comboBox_ = New System.Windows.Forms.ComboBox
            comboBox_.Width = 150
            comboBox_.DropDownStyle = ComboBoxStyle.DropDownList
            comboBox_.Text = transmitValue
            comboBox_.Enabled = enabled

            selectedItem_ = selectedItem
            ReDim items_(items.Length - 1)
            For i As Long = 0 To items.Length - 1
                items_(i) = items(i)
            Next i

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New ComboBox(label_.Text, selectedItem_, comboBox_.Text, comboBox_.Enabled, items_)

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(label_.Text, "label")
            serializer.serializeAttribute(selectedItem_, "selectedItem", "")
            serializer.serializeAttribute(comboBox_.Enabled, "enabled", "True")
            serializer.serializePropertyList(items_, "Items", "Item")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' StartupParameter Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer) Implements StartupParameter.draw

            label_.Location = New Point(10, y)
            label_.Enabled = comboBox_.Enabled
            c.Add(label_)

            comboBox_.Items.Clear()
            For Each s As String In items_
                comboBox_.Items.Add(s)
            Next s
            comboBox_.SelectedItem = selectedItem_
            comboBox_.Location = New Point(150, y)
            c.Add(comboBox_)

        End Sub

        Private Sub SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles comboBox_.SelectedIndexChanged

            selectedItem_ = comboBox_.SelectedItem

        End Sub

    End Class

    Public Class DateTimePicker

        Implements ICloneable
        Implements ISerializable
        Implements StartupParameter

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private label_ As System.Windows.Forms.Label
        Private dateTimePicker_ As System.Windows.Forms.DateTimePicker

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "DateTimePicker"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = label_.Text
            End Get

            Set(ByVal value As String)
                label_.Text = value
            End Set

        End Property

        Public ReadOnly Property parameterName() As String Implements StartupParameter.parameterName

            Get
                parameterName = label_.Text
            End Get

        End Property

        Public ReadOnly Property typeName() As String Implements StartupParameter.typeName

            Get
                typeName = nodeName
            End Get

        End Property

        Public Property displayValue() As Object Implements StartupParameter.displayValue

            Get
                displayValue = dateTimePicker_.Value.ToString("yyyy-MM-dd")
            End Get

            Set(ByVal v As Object)
                dateTimePicker_.Value = v
            End Set

        End Property

        Public ReadOnly Property transmitValue() As Object Implements StartupParameter.transmitValue

            Get
                transmitValue = displayValue
            End Get

        End Property

        Public ReadOnly Property height() As Integer Implements StartupParameter.height

            Get
                height = 25
            End Get

        End Property

        Public ReadOnly Property visible() As Boolean Implements StartupParameter.visible

            Get
                visible = True
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

            label_ = New System.Windows.Forms.Label
            label_.Width = 140
            dateTimePicker_ = New System.Windows.Forms.DateTimePicker()
            dateTimePicker_.Width = 150
            dateTimePicker_.Format = DateTimePickerFormat.Custom
            dateTimePicker_.CustomFormat = "dd-MMM-yyyy"

        End Sub

        Public Sub New(ByVal label As String, ByVal value As Date, ByVal enabled As Boolean)

            label_ = New System.Windows.Forms.Label
            label_.Width = 140
            label_.Text = label
            dateTimePicker_ = New System.Windows.Forms.DateTimePicker()
            dateTimePicker_.Width = 150
            dateTimePicker_.Format = DateTimePickerFormat.Custom
            dateTimePicker_.CustomFormat = "dd-MMM-yyyy"
            dateTimePicker_.Value = value
            dateTimePicker_.Enabled = enabled

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New DateTimePicker(label_.Text, dateTimePicker_.Value, dateTimePicker_.Enabled)

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(label_.Text, "label")
            serializer.serializeAttribute(dateTimePicker_.Value, "value")
            serializer.serializeAttribute(dateTimePicker_.Enabled, "enabled", "True")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' StartupParameter Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer) Implements StartupParameter.draw

            label_.Location = New Point(10, y)
            label_.Enabled = dateTimePicker_.Enabled
            c.Add(label_)

            dateTimePicker_.Location = New Point(150, y)
            c.Add(dateTimePicker_)

        End Sub

    End Class

End Namespace
