uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetApiVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetApiVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetApiVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetApiVersion fails it will raise an exception
    end;
  end;
end;
