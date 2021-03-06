(defun sql-server-sproc ()
  "Create a new sql server stored procedure"
  (interactive "*")
  (setq sprocName (read-string "Sproc name: "))
  (insert "IF EXISTS (SELECT * FROM dbo.sysobjects WHERE Name='" sprocName "' AND xtype='P')\n")
  (insert "DROP PROCEDURE [dbo].[" sprocName "]\n")
  (insert "\nGO\n\n")
  (insert "CREATE PROCEDURE [dbo].[" sprocName "]\n")
  (insert "<PARAMS HERE>\n")
  (insert "AS\n")
  (insert "BEGIN\n\n")
  (insert "END\n")
  (insert "\nGO\n\n")
  (search-backward "<")
  (kill-line)
 )

(defun sql-server-function ()
  "Create a new sql server function"
  (interactive "*")
  (setq funcName (read-string "Function name: "))
  (insert "IF EXISTS (SELECT * FROM dbo.SysObjects WHERE Name='" funcName "' AND xtype='FN')\n")
  (insert "DROP FUNCTION [dbo].[" funcName "]\n")
  (insert "\nGO\n\n")
  (insert "CREATE FUNCTION [dbo].[" funcName "]\n")
  (insert "(\n")
  (insert "<PARAMS HERE>\n")
  (insert ")\n")
  (insert "RETURNS\n")
  (insert "AS\n")
  (insert "BEGIN\n\n")
  (insert "END\n")
  (insert "\nGO\n\n")
  (search-backward "<")
  (kill-line)
)

(defun sql-server-table ()
  "Create a new sql server table"
  (interactive "*")
  (setq tableName (read-string "Table name: "))
   (insert "IF NOT EXISTS (SELECT 1 FROM SysObjects WHERE Name='" tableName "' AND Type='U')\n")
	(insert "CREATE TABLE [dbo].[" tableName "](\n")
	(insert "<DEFINITION HERE>\n")
	(insert ") ON [PRIMARY]\n")
	(insert "\nGO\n")
	(search-backward "<")
	(kill-line)
)

(defun sql-server-view-exists ()
  "Check if a sql server view does not exist"
  (interactive "*")
  (setq vName (read-string "View name: "))
  (insert "IF NOT EXISTS (SELECT * FROM dbo.SysObjects WHERE Name='" vName "' AND xtype='V')\n")
  (insert "BEGIN\n")
  (insert "<DEFINITION HERE>\n")
  (insert "END\n")
  (insert "\nGO\n\n")
  (search-backward "<")
  (kill-line)
)

(defun sql-server-trigger ()
  "create a new Sql Server trigger"
  (interactive "*")
  (setq vName (read-string "Trigger name: "))
  (setq tName (read-string "Table name: "))
  (insert "IF dbo.TriggerExists('" tName "','" vName "')=0\n")
  (insert "BEGIN\n")
  (insert "CREATE TRIGGER dbo." vName "\n")
  (insert "ON dbo." tName "\n")
  (insert "AFTER INSERT, UPDATE\n")
  (insert "AS\n")
  (insert "BEGIN\n")
  (insert "<DEFINITION HERE>\n")
  (insert "END\n")
  (insert "\nGO\n\n")
  (search-backward "<")
  (kill-line)
)
