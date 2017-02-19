uses
  qBitTorrentWebUI;

type
  TForm1 = class(TForm)
    {...}
    Fqb: TqBitTorrentWebUI;
    {...}
  public
    {...}
    procedure GetMinApiVersionExecute(Sender: TObject);
    {...}
  end;

var
  Form1: TForm1;

implementation

prodecure TForm1.GetMinApiVersionExecute(Sender: TObject);
var
  sVersion: String;
begin
  try
    sversion := Fqb.GetMinApiVersion
     // Do something with the version
  except
    on E:Exception do
    begin
      // If GetMinApiVersion fails it will raise an exception
    end;
  end;
end;
