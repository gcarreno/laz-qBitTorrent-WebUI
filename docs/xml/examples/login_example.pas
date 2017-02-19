uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure LoginExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.LoginExecute(Sender: TObject);
begin
  try
    if Fqb.Login then
    begin
      // Do something after a successfull login
    end;
  except
    on E:Exception do
    begin
      // If login fails it will raise an exception
    end;
  end;
end;
