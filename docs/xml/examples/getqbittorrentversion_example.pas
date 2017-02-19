uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetqBitTorrentVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetqBitTorrentVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetqBitTorrentVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetqBitTorrentVersion fails it will raise an exception
    end;
  end;
end;
