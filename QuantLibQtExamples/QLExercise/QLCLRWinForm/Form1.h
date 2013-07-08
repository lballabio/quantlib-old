

#pragma once
//#include "EquityOptionRework.hpp"
#include "BlackScholesCalculator.h"
#include <ql/quantlib.hpp>
#include <msclr/marshal_cppstd.h>


namespace CLRWinForm {

	using namespace System;
	using namespace System::ComponentModel;
	using namespace System::Collections;
	using namespace System::Windows::Forms;
	using namespace System::Data;
	using namespace System::Data::SqlClient;
	using namespace System::Data::SQLite;
	using namespace System::Drawing;

	using namespace QuantLib;

	/// <summary>
	/// Form1 の概要
	/// </summary>
	public ref class Form1 : public System::Windows::Forms::Form
	{
	public:
		Form1(void)
		{
			InitializeComponent();
			//
			//TODO: ここにコンストラクター コードを追加します
			//
			
			// How to: Bind Data to the Windows Forms DataGridView Control
			// http://msdn.microsoft.com/en-us/library/fbk67b6z.aspx

			dataGridView1 = gcnew DataGridView();
			bindingSource1 = gcnew BindingSource();
			sqLiteDataAdapter1 = gcnew System::Data::SQLite::SQLiteDataAdapter(); 
			reloadButton = gcnew Button();
			submitButton = gcnew Button();

			dataGridView1->Dock = DockStyle::Fill;

			reloadButton->Text = "reload";
			submitButton->Text = "submit";
			reloadButton->Click += gcnew System::EventHandler(this,&Form1::reloadButton_Click);
			submitButton->Click += gcnew System::EventHandler(this,&Form1::submitButton_Click);

			FlowLayoutPanel^ panel = gcnew FlowLayoutPanel();
			panel->Dock = DockStyle::Top;
			panel->AutoSize = true;
			panel->Controls->AddRange(gcnew array<Control^> { reloadButton, submitButton });

			this->Controls->AddRange(gcnew array<Control^> { dataGridView1, panel });
			this->Load += gcnew System::EventHandler(this,&Form1::Form1_Load);

		}

	protected:
		/// <summary>
		/// 使用中のリソースをすべてクリーンアップします。
		/// </summary>
		~Form1()
		{
			if (components)
			{
				delete components;
			}
		}
	private: System::Windows::Forms::Button^  button1;
	private: System::Windows::Forms::DataGridView^  dataGridView1;


	private: System::Windows::Forms::StatusStrip^  statusStrip1;
	private: System::Windows::Forms::TableLayoutPanel^  tableLayoutPanel1;
	private: System::Windows::Forms::BindingSource^  bindingSource1;
	private: System::Data::SQLite::SQLiteCommand^  sqliteSelectCommand1;
	private: System::Data::SQLite::SQLiteConnection^  sqLiteConnection1;
	private: System::Data::SQLite::SQLiteCommand^  sqliteInsertCommand1;
	private: System::Data::SQLite::SQLiteCommand^  sqliteUpdateCommand1;
	private: System::Data::SQLite::SQLiteCommand^  sqliteDeleteCommand1;
	private: System::Data::SQLite::SQLiteDataAdapter^  sqLiteDataAdapter1;
	private: System::Windows::Forms::Button^  reloadButton;
	private: System::Windows::Forms::Button^  submitButton;
	private: System::ComponentModel::IContainer^  components;
	protected: 

	private:
		/// <summary>
		/// 必要なデザイナー変数です。
		/// </summary>


#pragma region Windows Form Designer generated code
		/// <summary>
		/// デザイナー サポートに必要なメソッドです。このメソッドの内容を
		/// コード エディターで変更しないでください。
		/// </summary>
		void InitializeComponent(void)
		{
			this->components = (gcnew System::ComponentModel::Container());
			System::ComponentModel::ComponentResourceManager^  resources = (gcnew System::ComponentModel::ComponentResourceManager(Form1::typeid));
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter1 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter2 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter3 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter4 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter5 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter6 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter7 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter8 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter9 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter10 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter11 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter12 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter13 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter14 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter15 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter16 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter17 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter18 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter19 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter20 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter21 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter22 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter23 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter24 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter25 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter26 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter27 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter28 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter29 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter30 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter31 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter32 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter33 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter34 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter35 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter36 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter37 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter38 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter39 = (gcnew System::Data::SQLite::SQLiteParameter());
			System::Data::SQLite::SQLiteParameter^  sqLiteParameter40 = (gcnew System::Data::SQLite::SQLiteParameter());
			this->button1 = (gcnew System::Windows::Forms::Button());
			this->dataGridView1 = (gcnew System::Windows::Forms::DataGridView());
			this->statusStrip1 = (gcnew System::Windows::Forms::StatusStrip());
			this->tableLayoutPanel1 = (gcnew System::Windows::Forms::TableLayoutPanel());
			this->reloadButton = (gcnew System::Windows::Forms::Button());
			this->submitButton = (gcnew System::Windows::Forms::Button());
			this->bindingSource1 = (gcnew System::Windows::Forms::BindingSource(this->components));
			this->sqliteSelectCommand1 = (gcnew System::Data::SQLite::SQLiteCommand());
			this->sqLiteConnection1 = (gcnew System::Data::SQLite::SQLiteConnection());
			this->sqliteInsertCommand1 = (gcnew System::Data::SQLite::SQLiteCommand());
			this->sqliteUpdateCommand1 = (gcnew System::Data::SQLite::SQLiteCommand());
			this->sqliteDeleteCommand1 = (gcnew System::Data::SQLite::SQLiteCommand());
			this->sqLiteDataAdapter1 = (gcnew System::Data::SQLite::SQLiteDataAdapter());
			(cli::safe_cast<System::ComponentModel::ISupportInitialize^  >(this->dataGridView1))->BeginInit();
			this->tableLayoutPanel1->SuspendLayout();
			(cli::safe_cast<System::ComponentModel::ISupportInitialize^  >(this->bindingSource1))->BeginInit();
			this->SuspendLayout();
			// 
			// button1
			// 
			this->button1->Location = System::Drawing::Point(3, 3);
			this->button1->Name = L"button1";
			this->button1->Size = System::Drawing::Size(75, 23);
			this->button1->TabIndex = 0;
			this->button1->Text = L"button1";
			this->button1->UseVisualStyleBackColor = true;
			this->button1->Click += gcnew System::EventHandler(this, &Form1::button1_Click);
			// 
			// dataGridView1
			// 
			this->dataGridView1->ColumnHeadersHeightSizeMode = System::Windows::Forms::DataGridViewColumnHeadersHeightSizeMode::AutoSize;
			this->dataGridView1->Dock = System::Windows::Forms::DockStyle::Fill;
			this->dataGridView1->Location = System::Drawing::Point(3, 48);
			this->dataGridView1->Name = L"dataGridView1";
			this->dataGridView1->RowTemplate->Height = 21;
			this->dataGridView1->Size = System::Drawing::Size(503, 349);
			this->dataGridView1->TabIndex = 1;
			// 
			// statusStrip1
			// 
			this->statusStrip1->Location = System::Drawing::Point(0, 424);
			this->statusStrip1->Name = L"statusStrip1";
			this->statusStrip1->Size = System::Drawing::Size(666, 22);
			this->statusStrip1->TabIndex = 2;
			this->statusStrip1->Text = L"statusStrip1";
			// 
			// tableLayoutPanel1
			// 
			this->tableLayoutPanel1->ColumnCount = 2;
			this->tableLayoutPanel1->ColumnStyles->Add((gcnew System::Windows::Forms::ColumnStyle(System::Windows::Forms::SizeType::Percent, 
				79.28349F)));
			this->tableLayoutPanel1->ColumnStyles->Add((gcnew System::Windows::Forms::ColumnStyle(System::Windows::Forms::SizeType::Percent, 
				20.71651F)));
			this->tableLayoutPanel1->Controls->Add(this->button1, 0, 0);
			this->tableLayoutPanel1->Controls->Add(this->dataGridView1, 0, 1);
			this->tableLayoutPanel1->Controls->Add(this->reloadButton, 1, 0);
			this->tableLayoutPanel1->Controls->Add(this->submitButton, 1, 1);
			this->tableLayoutPanel1->Location = System::Drawing::Point(12, 12);
			this->tableLayoutPanel1->Name = L"tableLayoutPanel1";
			this->tableLayoutPanel1->RowCount = 2;
			this->tableLayoutPanel1->RowStyles->Add((gcnew System::Windows::Forms::RowStyle(System::Windows::Forms::SizeType::Percent, 11.30137F)));
			this->tableLayoutPanel1->RowStyles->Add((gcnew System::Windows::Forms::RowStyle(System::Windows::Forms::SizeType::Percent, 88.69863F)));
			this->tableLayoutPanel1->Size = System::Drawing::Size(642, 400);
			this->tableLayoutPanel1->TabIndex = 3;
			// 
			// reloadButton
			// 
			this->reloadButton->Location = System::Drawing::Point(512, 3);
			this->reloadButton->Name = L"reloadButton";
			this->reloadButton->Size = System::Drawing::Size(127, 23);
			this->reloadButton->TabIndex = 2;
			this->reloadButton->Text = L"reloadButton";
			this->reloadButton->UseVisualStyleBackColor = true;
			this->reloadButton->Click += gcnew System::EventHandler(this, &Form1::reloadButton_Click);
			// 
			// submitButton
			// 
			this->submitButton->Location = System::Drawing::Point(512, 48);
			this->submitButton->Name = L"submitButton";
			this->submitButton->Size = System::Drawing::Size(127, 23);
			this->submitButton->TabIndex = 3;
			this->submitButton->Text = L"submitButton";
			this->submitButton->UseVisualStyleBackColor = true;
			this->submitButton->Click += gcnew System::EventHandler(this, &Form1::submitButton_Click);
			// 
			// sqliteSelectCommand1
			// 
			this->sqliteSelectCommand1->CommandText = L"SELECT            id, year, month, day, customer_name, product_name, unit_price, " 
				L"qty, created_at, updated_at\r\nFROM              order_records";
			this->sqliteSelectCommand1->Connection = this->sqLiteConnection1;
			// 
			// sqLiteConnection1
			// 
			this->sqLiteConnection1->ConnectionString = L"data source=C:\\QuantLib\\QuantLib-1.2\\Examples\\QLExercise\\QLCLRWinForm\\mydb.db";
			this->sqLiteConnection1->DefaultTimeout = 30;
			// 
			// sqliteInsertCommand1
			// 
			this->sqliteInsertCommand1->CommandText = resources->GetString(L"sqliteInsertCommand1.CommandText");
			this->sqliteInsertCommand1->Connection = this->sqLiteConnection1;
			sqLiteParameter1->ParameterName = L"@year";
			sqLiteParameter1->SourceColumn = L"year";
			sqLiteParameter2->ParameterName = L"@month";
			sqLiteParameter2->SourceColumn = L"month";
			sqLiteParameter3->ParameterName = L"@day";
			sqLiteParameter3->SourceColumn = L"day";
			sqLiteParameter4->ParameterName = L"@customer_name";
			sqLiteParameter4->SourceColumn = L"customer_name";
			sqLiteParameter5->ParameterName = L"@product_name";
			sqLiteParameter5->SourceColumn = L"product_name";
			sqLiteParameter6->ParameterName = L"@unit_price";
			sqLiteParameter6->SourceColumn = L"unit_price";
			sqLiteParameter7->ParameterName = L"@qty";
			sqLiteParameter7->SourceColumn = L"qty";
			sqLiteParameter8->ParameterName = L"@created_at";
			sqLiteParameter8->SourceColumn = L"created_at";
			sqLiteParameter9->ParameterName = L"@updated_at";
			sqLiteParameter9->SourceColumn = L"updated_at";
			this->sqliteInsertCommand1->Parameters->AddRange(gcnew cli::array< System::Data::SQLite::SQLiteParameter^  >(9) {sqLiteParameter1, 
				sqLiteParameter2, sqLiteParameter3, sqLiteParameter4, sqLiteParameter5, sqLiteParameter6, sqLiteParameter7, sqLiteParameter8, 
				sqLiteParameter9});
			// 
			// sqliteUpdateCommand1
			// 
			this->sqliteUpdateCommand1->CommandText = resources->GetString(L"sqliteUpdateCommand1.CommandText");
			this->sqliteUpdateCommand1->Connection = this->sqLiteConnection1;
			sqLiteParameter10->ParameterName = L"@year";
			sqLiteParameter10->SourceColumn = L"year";
			sqLiteParameter11->ParameterName = L"@month";
			sqLiteParameter11->SourceColumn = L"month";
			sqLiteParameter12->ParameterName = L"@day";
			sqLiteParameter12->SourceColumn = L"day";
			sqLiteParameter13->ParameterName = L"@customer_name";
			sqLiteParameter13->SourceColumn = L"customer_name";
			sqLiteParameter14->ParameterName = L"@product_name";
			sqLiteParameter14->SourceColumn = L"product_name";
			sqLiteParameter15->ParameterName = L"@unit_price";
			sqLiteParameter15->SourceColumn = L"unit_price";
			sqLiteParameter16->ParameterName = L"@qty";
			sqLiteParameter16->SourceColumn = L"qty";
			sqLiteParameter17->ParameterName = L"@created_at";
			sqLiteParameter17->SourceColumn = L"created_at";
			sqLiteParameter18->ParameterName = L"@updated_at";
			sqLiteParameter18->SourceColumn = L"updated_at";
			sqLiteParameter19->ParameterName = L"@Original_id";
			sqLiteParameter19->SourceColumn = L"id";
			sqLiteParameter19->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter20->ParameterName = L"@Original_year";
			sqLiteParameter20->SourceColumn = L"year";
			sqLiteParameter20->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter21->ParameterName = L"@Original_month";
			sqLiteParameter21->SourceColumn = L"month";
			sqLiteParameter21->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter22->ParameterName = L"@Original_day";
			sqLiteParameter22->SourceColumn = L"day";
			sqLiteParameter22->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter23->ParameterName = L"@Original_customer_name";
			sqLiteParameter23->SourceColumn = L"customer_name";
			sqLiteParameter23->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter24->ParameterName = L"@Original_product_name";
			sqLiteParameter24->SourceColumn = L"product_name";
			sqLiteParameter24->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter25->ParameterName = L"@Original_unit_price";
			sqLiteParameter25->SourceColumn = L"unit_price";
			sqLiteParameter25->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter26->ParameterName = L"@Original_qty";
			sqLiteParameter26->SourceColumn = L"qty";
			sqLiteParameter26->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter27->ParameterName = L"@Original_created_at";
			sqLiteParameter27->SourceColumn = L"created_at";
			sqLiteParameter27->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter28->ParameterName = L"@IsNull_updated_at";
			sqLiteParameter28->SourceColumn = L"updated_at";
			sqLiteParameter28->SourceColumnNullMapping = true;
			sqLiteParameter28->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter29->ParameterName = L"@Original_updated_at";
			sqLiteParameter29->SourceColumn = L"updated_at";
			sqLiteParameter29->SourceVersion = System::Data::DataRowVersion::Original;
			this->sqliteUpdateCommand1->Parameters->AddRange(gcnew cli::array< System::Data::SQLite::SQLiteParameter^  >(20) {sqLiteParameter10, 
				sqLiteParameter11, sqLiteParameter12, sqLiteParameter13, sqLiteParameter14, sqLiteParameter15, sqLiteParameter16, sqLiteParameter17, 
				sqLiteParameter18, sqLiteParameter19, sqLiteParameter20, sqLiteParameter21, sqLiteParameter22, sqLiteParameter23, sqLiteParameter24, 
				sqLiteParameter25, sqLiteParameter26, sqLiteParameter27, sqLiteParameter28, sqLiteParameter29});
			// 
			// sqliteDeleteCommand1
			// 
			this->sqliteDeleteCommand1->CommandText = resources->GetString(L"sqliteDeleteCommand1.CommandText");
			this->sqliteDeleteCommand1->Connection = this->sqLiteConnection1;
			sqLiteParameter30->ParameterName = L"@Original_id";
			sqLiteParameter30->SourceColumn = L"id";
			sqLiteParameter30->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter31->ParameterName = L"@Original_year";
			sqLiteParameter31->SourceColumn = L"year";
			sqLiteParameter31->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter32->ParameterName = L"@Original_month";
			sqLiteParameter32->SourceColumn = L"month";
			sqLiteParameter32->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter33->ParameterName = L"@Original_day";
			sqLiteParameter33->SourceColumn = L"day";
			sqLiteParameter33->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter34->ParameterName = L"@Original_customer_name";
			sqLiteParameter34->SourceColumn = L"customer_name";
			sqLiteParameter34->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter35->ParameterName = L"@Original_product_name";
			sqLiteParameter35->SourceColumn = L"product_name";
			sqLiteParameter35->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter36->ParameterName = L"@Original_unit_price";
			sqLiteParameter36->SourceColumn = L"unit_price";
			sqLiteParameter36->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter37->ParameterName = L"@Original_qty";
			sqLiteParameter37->SourceColumn = L"qty";
			sqLiteParameter37->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter38->ParameterName = L"@Original_created_at";
			sqLiteParameter38->SourceColumn = L"created_at";
			sqLiteParameter38->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter39->ParameterName = L"@IsNull_updated_at";
			sqLiteParameter39->SourceColumn = L"updated_at";
			sqLiteParameter39->SourceColumnNullMapping = true;
			sqLiteParameter39->SourceVersion = System::Data::DataRowVersion::Original;
			sqLiteParameter40->ParameterName = L"@Original_updated_at";
			sqLiteParameter40->SourceColumn = L"updated_at";
			sqLiteParameter40->SourceVersion = System::Data::DataRowVersion::Original;
			this->sqliteDeleteCommand1->Parameters->AddRange(gcnew cli::array< System::Data::SQLite::SQLiteParameter^  >(11) {sqLiteParameter30, 
				sqLiteParameter31, sqLiteParameter32, sqLiteParameter33, sqLiteParameter34, sqLiteParameter35, sqLiteParameter36, sqLiteParameter37, 
				sqLiteParameter38, sqLiteParameter39, sqLiteParameter40});
			// 
			// sqLiteDataAdapter1
			// 
			this->sqLiteDataAdapter1->DeleteCommand = this->sqliteDeleteCommand1;
			this->sqLiteDataAdapter1->InsertCommand = this->sqliteInsertCommand1;
			this->sqLiteDataAdapter1->SelectCommand = this->sqliteSelectCommand1;
			cli::array< System::Data::Common::DataColumnMapping^ >^ __mcTemp__1 = gcnew cli::array< System::Data::Common::DataColumnMapping^  >(10) {(gcnew System::Data::Common::DataColumnMapping(L"id", 
				L"id")), (gcnew System::Data::Common::DataColumnMapping(L"year", L"year")), (gcnew System::Data::Common::DataColumnMapping(L"month", 
				L"month")), (gcnew System::Data::Common::DataColumnMapping(L"day", L"day")), (gcnew System::Data::Common::DataColumnMapping(L"customer_name", 
				L"customer_name")), (gcnew System::Data::Common::DataColumnMapping(L"product_name", L"product_name")), (gcnew System::Data::Common::DataColumnMapping(L"unit_price", 
				L"unit_price")), (gcnew System::Data::Common::DataColumnMapping(L"qty", L"qty")), (gcnew System::Data::Common::DataColumnMapping(L"created_at", 
				L"created_at")), (gcnew System::Data::Common::DataColumnMapping(L"updated_at", L"updated_at"))};
			this->sqLiteDataAdapter1->TableMappings->AddRange(gcnew cli::array< System::Data::Common::DataTableMapping^  >(1) {(gcnew System::Data::Common::DataTableMapping(L"Table", 
				L"order_records", __mcTemp__1))});
			this->sqLiteDataAdapter1->UpdateCommand = this->sqliteUpdateCommand1;
			// 
			// Form1
			// 
			this->AutoScaleDimensions = System::Drawing::SizeF(6, 12);
			this->AutoScaleMode = System::Windows::Forms::AutoScaleMode::Font;
			this->ClientSize = System::Drawing::Size(666, 446);
			this->Controls->Add(this->tableLayoutPanel1);
			this->Controls->Add(this->statusStrip1);
			this->Name = L"Form1";
			this->Text = L"QuantLib WinForm";
			this->Load += gcnew System::EventHandler(this, &Form1::Form1_Load);
			(cli::safe_cast<System::ComponentModel::ISupportInitialize^  >(this->dataGridView1))->EndInit();
			this->tableLayoutPanel1->ResumeLayout(false);
			(cli::safe_cast<System::ComponentModel::ISupportInitialize^  >(this->bindingSource1))->EndInit();
			this->ResumeLayout(false);
			this->PerformLayout();

		}
#pragma endregion
	private: System::Void button1_Click(System::Object^  sender, System::EventArgs^  e) {
				System::Diagnostics::Debug::WriteLine("Button Clicked!");

				/*
				DateTime now = DateTime::Now;
				String^ msg_ = now.ToString();
				MessageBox::Show(msg_);
				*/


				GaussianCopula gaussCop(0.7);
				GumbelCopula gumbCop(1.7);
				Real x=0.7, y =0.2;
				double gausscopula = gaussCop(x,y);
				double gumbelcopula = gumbCop(x,y);
				//MessageBox::Show(System::Convert::ToString(gausscopula) + ", " + System::Convert::ToString(gumbelcopula));

				 
				std::string stringPart = "ABCDEFGHIJKLMN\n";
				int intPart = 10;
				double doublePart = 3.14165972;
				String^ msg = String::Concat("Message", msclr::interop::marshal_as<System::String^>(stringPart));
				//msg = String::Concat(msg, intPart);
				msg = String::Concat(msg, doublePart);
				//MessageBox::Show(msg);


				BSCalc bsc;
				double bsdelta;
				bsdelta = bsc.getdelta();
				MessageBox::Show("Black Scholes Put Option Delta:\n  " + System::Convert::ToString(bsdelta));
				bsc.calculate();
				bsdelta = bsc.getdelta();
				MessageBox::Show("Black Scholes Put Option Delta:\n  " + System::Convert::ToString(bsdelta));
			 
			}
	private: System::Void reloadButton_Click(System::Object^  sender, System::EventArgs^  e) {
				// Reload the data from the database.
				GetData(sqLiteDataAdapter1->SelectCommand->CommandText);
			}

	private: System::Void submitButton_Click(System::Object^  sender, System::EventArgs^  e) {
				// Update the database with the user's changes.
				sqLiteDataAdapter1->Update((DataTable^)bindingSource1->DataSource);
			}

	private:
		void GetData(String^ selectCommand)
		{
			try
			{
				// Specify a connection string. Replace the given value with a 
				// valid connection string for a Northwind SQL Server sample
				// database accessible to your system.

				// FIXME
				String^ connectionString = 
					"Integrated Security=SSPI;Persist Security Info=False;" +
					"Initial Catalog=Northwind;Data Source=localhost";

				// Create a new data adapter based on the specified query.
				//sqLiteDataAdapter1 = gcnew SqlDataAdapter(selectCommand, connectionString);
				sqLiteDataAdapter1 = gcnew SQLiteDataAdapter(selectCommand, connectionString);

				// Create a command builder to generate SQL update, insert, and
				// delete commands based on selectCommand. These are used to
				// update the database.

				// FIXME
				gcnew SqlCommandBuilder(sqLiteDataAdapter1);

				// Populate a new data table and bind it to the BindingSource.
				DataTable^ table = gcnew DataTable();
				sqLiteDataAdapter1->Fill(table);
				bindingSource1->DataSource = table;

				// Resize the DataGridView columns to fit the newly loaded content.
				dataGridView1->AutoResizeColumns( 
					DataGridViewAutoSizeColumnsMode::AllCellsExceptHeader);
			}
			catch (SqlException^)
			{
				MessageBox::Show("To run this example, replace the value of the " +
					"connectionString variable with a connection string that is " +
					"valid for your system.");
			}
		}
	private: System::Void Form1_Load(System::Object^  sender, System::EventArgs^  e) {
			 // Bind the DataGridView to the BindingSource
			 // and load the data from the database.
			 dataGridView1->DataSource = bindingSource1;
			 GetData("select * from Customers");
		 }
};
}


