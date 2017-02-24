uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure ExecShutdownExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.ExecShutdownExecute(Sender: TObject);
begin
  try
    try
      Fqb.Active := True;
      if Fqb.ExecShutdown then
      begin
        // Do something after a successfull shutdown
      end;
    except
      on E:Exception do
      begin
        // If shutdown fails it will raise an exception
      end;
    end;
  finally
    Fqb.Active := False;
  end;
end;
