uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure LogoutExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.LogoutExecute(Sender: TObject);
begin
  try
    if Fqb.Logout then
    begin
      // Do something after a successfull logout
    end;
  except
    on E:Exception do
    begin
      // If logout fails it will raise an exception
    end;
  end;
end;
