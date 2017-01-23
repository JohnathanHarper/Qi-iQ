{*
Dieses Modul enthält die Klasse TTXTController.

Autoren: Michael Gebing, Sven Stegemann

Datum: 25.11.2016

Copyright © 2015,2016 FH Münster
                      Fachbereich Maschinenbau
                      Labor für Systemanalyse und Optimierung
}

unit mTXTController;

interface

uses Winapi.Windows, Vcl.Dialogs, Vcl.Imaging.jpeg,
  Vcl.Graphics, SysUtils, Classes, IdTCPClient, Math,
  mSRWLock, mTXTRecordsUndKonstanten, IdGlobal, CodeSiteLogging, IdExceptionCore;

const
  ALLE_KANAELE = 4; // Empfängt die Signale von allen Fernbedienungen.

type
  TTXTController = class;

  {*
    Die Modi in die Eingänge versetzt werden können.
  }
  TEingangsmodus = (
    /// Der Eingang liefert die Spannung am Eingang als Integer im Intervall 0-1023.
    /// Dies ist die Standardeinstellung.
    SPANNUNG,
    /// Der Eingang liefert den Wert 1, wenn beide Kontakte leitend verbunden
    /// sind (z.B. Taster gedrückt), sonst 0.
    DIGITAL,
    /// Der Eingang liefert den Widerstand zwischen den beiden Kontakten
    /// des Eingangs als Integer im Intervall 0-1023. (z.B. Photowiderstand)
    ANALOG,
    /// Der Eingang liefert den Abstand in cm zu dem nächsten Objekt (Ultraschallsensor).
    ENTFERNUNG
  );

  {*
    Die Modi, in die Ausgänge versetzt werden können.
  }
  TAusgangsmodus = (
    /// Zwischen den beiden Anschlüssen kann ein PWM Signal zwischen -512 und 512
    /// ausgegeben werden. Dies ist die Standardeinstellung.
    MOTOR,
    /// Die beiden Anschlüsse können unabhängig voneinander angesteuert werden.
    ZWEI_AUSGAENGE
  );

  /// Für die Auswahl eines der Steuerknüppel der IR-Fernbedienung.
  TSteuerknueppelSeite = (
    LINKS,
    RECHTS
  );

  /// Wählt die Richtung des Steuerknüppels der IR-Fernbedienung.
  TSteuerknueppelRichtung = (
    HORIZONTAL,
    VERTIKAL
  );

  /// Unterscheidet die Schalter in der Mitte der IR-Fernbedienung.
  TFernbedienungsSchalter = (
    AN,
    AUS,
    SCHALTER1,
    SCHALTER2
  );

  {*
        Ermöglicht die Konfiguration der Ein- und Ausgände des Controllers in einem
        "fluent interface".
        Nicht angegebene Ein- oder Ausgänge behalten die letzten ihnen zugewiesenen
        Modi bzw. die Standardmodi, falls noch keine Modi gesetzt wurden.
        Wird Senden nicht aufgerufen, so werden die Änderungen an der Konfiguration
        trotzdem gespeichert und können zu einem späteren Zeitpunkt an den
        Controller gesendet werden.

        Beispielcode:

        @longCode(#
          MeinController.Konfiguration
            .SetzeEingangsmodus(2, ENTFERNUNG) // Eingang I2 als Ultraschallsensor
            .SetzeEingangsmodus(5, DIGITAL) // Eingang I5 z. B. als Taster
            .SetzeEingangsmodus(7, ANALOG) // Eingang I7 z. B. als Fotowiderstand
            .SetzeAusgangsmodus(1, MOTOR) // Motor M1
            .SetzeAusgangsmodus(2, MOTOR) // Motor M2
            .SetzeAusgangsmodus(4, ZWEI_AUSGÄNGE) // Ausgänge O7 und O8
            .Senden;
        #)
  }
  TTXTController_Konfiguration = class(TObject)
    public
      /// Erzeugt eine neue Konfiguration.
      /// Sollte nur im Konstruktor von TTXTController aufgerufen werden.
      ///
      /// @param roboter Der Controller zu dem die Konfiguration gehört.
      constructor Create(roboter: TTXTController);

      /// Setzt den Modus des Eingangs I<Eingangsnummer>.
      /// Für eine Übersicht möglicher Modi siehe TEingangsmodus.
      ///
      /// @returns Gibt das Konfigurationsobjekt selbst zurück.
      function SetzeEingangsmodus(Eingangsnummer: Integer; Modus: TEingangsmodus)
          : TTXTController_Konfiguration;

      /// Setzt den Modus des Motors M<Motornummer> bzw. der Ausgänge
      /// O<Motornummer*2-1> und O<Motornummer*2>.
      /// Für eine Übersicht möglicher Modi siehe TAusgangsmodus.
      ///
      /// @returns Gibt das Konfigurationsobjekt  selbst zurück.
      function SetzeAusgangsmodus(Motornummer: Integer; Modus: TAusgangsmodus)
          : TTXTController_Konfiguration;

      /// Sendet die Konfiguration an den Controller.
      function Senden: Boolean;

    private
      /// Der Controller zu dem diese Konfiguration gehört.
      roboter: TTXTController;

      /// Das Netzwerkpaket, das an den Controller geschickt wird.
      paket: ftIF2013Command_UpdateConfig;

      /// Ein Lock, das den Zugriff auf paket regelt.
      lock: TSRWLock;
  end;

  {*
        Ermöglicht es Steuerbefehle an den Controller in einem "fluent interface"
        zu schicken.
        Es müssen nur die Befehle angegeben werden, die gegenüber dem letzten
        Aufruf geändert werden sollen.

        Beispielcode:

        @longCode(#
          MeinController.Steuerbefehl
            .SetzeMotorsynchronisation(1, 2) // M1 wird zu M2 synchronisiert.
            .SetzeMotorgeschwindigkeit(1, 512) // M1 fährt mit voller Geschwindigkeit vorwärts.
            .SetzeMotorgeschwindigkeit(2, 512) // M2 fährt mit voller Geschwindigkeit vorwärts.
            .SetzeMotorgeschwindigkeit(3, -256) // M3 fährt mit halber Geschwindigkeit rückwärts.
            .SetzeMotorschrittzahl(3, 100) // M3 hört nach 100 Schritten auf, sich zu drehen.
            .SetzeAusgang(5, 128) // O5 liefert ein PWM-Signal mit einem Tastgrad von 25% (128/512).
            .SetzeGeraeusch(2, 3) // Spielt Geräusch Nummer 2 dreimal ab.
            .Senden;
        #)
  }
  TTXTController_Steuerbefehl = class(TObject)
    public
      /// Erzeugt eine neue Konfiguration.
      /// Sollte nur im Konstruktor von TTXTController aufgerufen werden.
      constructor Create(roboter: TTXTController);

      /// Setzt die Geschwindigkeit des Motors M<Motornummer>.
      /// @param Geschwindigkeit Motorgeschwindigkeit im Bereich von -512 bis +512.
      function SetzeMotorgeschwindigkeit
        (Motornummer: Integer; Geschwindigkeit: Integer)
        :TTXTController_Steuerbefehl;

      /// Bewegt den Motor M<Motornummer> um die angegebene Schrittzahl.
      /// Wenn Schrittzahl gleich 0 ist, wird der Motor bewegt, bis der nächste
      /// Befehl für diesen Motor geschickt wird.
      /// (kann zum Beispiel für Regelkreise benutzt werden)
      function SetzeMotorschrittzahl (Motornummer, Schrittzahl: Integer)
        :TTXTController_Steuerbefehl;

      /// Synchronisiert Motor M<Motornummer1> zu Motor M<Motornummer2>.
      function SetzeMotorsynchronisation (Motornummer1, Motornummer2: Integer)
        :TTXTController_Steuerbefehl;

      /// Macht die Synchronisation des Motors M<Motornummer> rückgängig.
      /// Als Motornummer muss die Nummer übergeben werden, die bei
      /// SetzeMotorsynchronisation als erster Parameter übergeben wurde.
      function HebeMotorsynchronisationAuf (Motornummer: Integer)
        : TTXTController_Steuerbefehl;

      /// Setzt den PWM-Wert von Ausgang O<Ausgangsnummer>.
      ///
      /// @param PWM-Wert Stärke des Signals im Bereich von 0 bis 512.
      function SetzeAusgang(Ausgangsnummer: Integer; PWM_Wert: Integer)
        : TTXTController_Steuerbefehl;

      /// Spielt das Geräusch mit der angebenen Nummer <Wiederholungen>-mal ab.
      /// Falls Wiederholungen gleich 0 ist wird das Geräusch wiederholt, bis
      /// ein neuer Befehl für ein Geräusch gesendet wird.
      function SetzeGeraeusch(Geraeusch: Integer; Wiederholungen: Integer=1)
        : TTXTController_Steuerbefehl;

      /// Sendet den Steuerbefehl an den Controller.
      /// Gibt True zurück, falls der Befehl erfolgreich gesendet wurde.
      function Senden: Boolean;

    private
      /// Der Controller dessen Konfiguration geändert wird.
      roboter: TTXTController;

      /// Das Netzwerkpaket, das an den Controller geschickt wird.
      paket: ftIF2013Command_ExchangeData;

      /// Ein Lock, das den Zugriff auf paket regelt.
      lock: TSRWLock;

      /// Gibt die m_motor_command_id des letzten Fahrbefehls an.
      /// (Muss erhöht werden, werden, falls mindestens ein Ausgangswert verändert wird)
      letzte_motor_id: array[0..3] of UINT16;

      /// Gibt die m_sound_command_id des letzten Fahrbefehls an.
      /// (Muss erhöht werden, werden, falls mindestens ein Ausgangswert verändert wird)
      letzte_sound_id: UINT16;
  end;

  {*
    Die Grundklasse zur Steuerung des TXT-Controllers.
    Diese Klasse stellt die Funktionen des TXT-Protokolls zur Verfügung.

    !! Achtung: Es ist zu beachten, dass alle Funktionen, die auf Eingänge, Zähler,
    Fernbedienungsdaten oder ähnliches zugreifen, immer die Werte liefern, die der
    Controller bei dem letzten Aufruf von LiesSensordaten oder Steuerbefehl.Senden
    gesendet hat. !!
  }
  TTXTController = class(TObject)
    public
      {*
       Erzeugt eine Verbindung zu einem Controller.

       @param IP_Adresse Die IP-Adresse des Controllers
      }
      constructor Create(IP_Adresse: String);

      {*
       Trennt alle Verbindungen zu dem Controller und gibt das Objekt frei.
      }
      destructor Destroy; override;

      {*
       Baut eine TCP-Verbindung zum Controller auf, wechselt in den Online-Modus
       und ruft SendeKonfig auf. TODO Funktionalität aufteilen?
      }
      function Verbinden: Boolean;

      {*
       Beendet den Online-Modus und trennt die TCP-Verbindung.
      }
      function Trennen: Boolean;

      {*
       True, wenn eine Verbindung zu dem Controller besteht.
       Dies ist gegeben falls Verbinden aufgerufen wurde und weder Trennen
       aufgerufen wurde, noch ein Verbindungsabbruch aufgetreten ist.
      }
      function IstVerbunden: Boolean;

      {*
        Startet die Kamera.
        Die Bilder können mit EmpfangeBild vom Controller geholt und mit
        KamerabildJpeg oder KamerabildBitmap gelesen werden.

        @return True, wenn keine Fehler aufgetreten sind.
      }
      function StarteKamera: Boolean;

      {*
        Stoppt die Kamera.

        @return True, wenn keine Fehler aufgetreten sind.
      }
      function StoppeKamera: Boolean;

      {*
       Empfängt und speichert die Daten aller Eingänge, Counter, etc.
       Ist äquivalent zu meinController.Steuerbefehl.Senden;
       (erneutes Senden des letzten Steuerpakets).
       Gibt bei Ergfolg true zurück.
      }
      function LiesSensordaten: Boolean;

      {*
        Empfängt ein Bild und schickt eine Empfangsbestätigung an den Controller.

        !Bug: Manchmal sendet der Controller kein Bild
        @return True, wenn keine Fehler aufgetreten sind.
      }
      function EmpfangeBild: Boolean;

      {*
       Gibt eine Referenz auf die Konfiguration des Controllers zurück
       Für eine Anleitung die Konfiguration geändert werden kann
       siehe TTXTController_Konfiguration.
      }
      function Konfiguration: TTXTController_Konfiguration;

      {*
       Gibt eine Referenz auf den aktuellen Steuerbefehl des Controllers zurück.
       Für eine Anleitung wie Steuerbefehle gesendet werden können
       siehe TTXTController_Steuerbefehl.

       Außerdem wird die Funktion von LiesSensordaten ausgeführt.
      }
      function Steuerbefehl: TTXTController_Steuerbefehl;

      {*
       Fragt einige Informationen des Controllers ab (Version, etc.)
      }
      function HoleStatus: ftIF2013Response_QueryStatus;

      {*
        Gibt das als letztes empfangene Kamerabild als TJpegImage zurück.
        KamerabildJpeg eignet sich zum Anzeigen von Bildern,
        KamerabildBitmap hingegen zum Zugriff auf Pixeldaten (z.B. für Bilderkennung)
        Damit Kamerabilder übertragen werden, muss StarteKamera aufgerufen werden.
      }
      function KamerabildJpeg: TJpegImage;

      {*
        Gibt das als letztes empfangene Kamerabild als TBitmap zurück.
        KamerabildJpeg eignet sich zum Anzeigen von Bildern,
        KamerabildBitmap hingegen zum Zugriff auf Pixeldaten (z.B. für Bilderkennung)
        Damit Kamerabilder übertragen werden, muss StarteKamera aufgerufen werden.

        !Bug: Das Rendern des JPEG-Kamerabildes in eine Bitmap erzeugt manchmal
              die Exception JPEG-Fehler #68.
      }
      function KamerabildBitmap: TBitmap;

      {*
        Gibt den Wert des Counters C<Counternummer> zurück.
      }
      function LiesCounter(Counternummer: Integer): Integer;

      {*
        Gibt den Zustand des digitalen Eingangs I<Eingangsnummer> zurück.
        Der Modus der Eingänge wird durch SendeKonfig gesetzt.
      }
      function LiesDigital(Eingangsnummer: Integer): Boolean;

      {*
        Gibt den Zustand des analogen Eingangs I<Eingangsnummer> zurück.
        Der Modus der Eingänge wird durch SendeKonfig gesetzt.
      }
      function LiesAnalog(Eingangsnummer: Integer): Integer;

      {*
       Liest die Position der Steuerknüppel der Infrarot-Fernbedienung aus
       dem fischertechnik Control Set.

       !! Diese Funktion wurde noch nicht getestet !!

       @param Kanal gibt an von welcher Fernbedienung gelesen werden soll.
                    Falls ALLE_KANAELE übergeben wird, werden die Daten von
                    allen (irgendeiner?) Fernbedienungen gelesen.
       @param Seite Gibt an, welcher Steuerknüppel gelesen wird.
       @param Richtung Gibt an in welche Richtung der Wert des Steuerknüppels
                       gelesen wird.
      }
      function LiesIrSteuerknueppelposition(Kanal: Integer; Seite: TSteuerknueppelSeite;
        Richtung: TSteuerknueppelRichtung): Integer;

      {*
       Liest einen der Schalter an einer der IR-Fernbedienungen aus.

       !! Diese Funktion wurde noch nicht getestet !!

       @param Kanal Gibt an von welcher Fernbedienung gelesen werden soll.
                    Falls ALLE_KANAELE übergeben wird, werden die Daten von
                    allen (irgendeiner?) Fernbedienungen gelesen.
       @param Schalter Gibt an welcher Schalter gelesen werden soll.
      }
      function LiesIrSchalter(Kanal: Integer; Schalter: TFernbedienungsSchalter): Boolean;

    private
      {*
       Die Konfiguration der Ein- und Ausgänge.
      }
      konfiguration_: TTXTController_Konfiguration;

      {*
       Der zuletzt an den Controller gesendete Befehl.
       Wird beim nächsten Aufruf von Steuerbefehl.Senden wieder gesendet, um
       dem Controller zu signalisieren, dass er den Befehl nicht abbrechen soll.
      }
      letzter_Befehl: TTXTController_Steuerbefehl;

      {*
       Die Informationen, die der Controller beim letzten LiesSensordaten oder
       Steuerbefehl.Senden geschickt hat.
      }
      antworten: ftIF2013Response_ExchangeData;

      {*
       Das vom Controller gesendete Kamerabild.
      }
      kamerabild: TJPEGImage;

      {*
       Der TCP-Client für die Kommunikation mit dem Controller (außer Kamerabilder)
      }
      IdTCPClient_Normal: TIdTCPClient;


      {*
       Der TCP-Client für die Übertragung der Kamerabilder
      }
      IdTCPClient_Kamera: TIdTCPClient;

      {*
       Regelt den Zugriff auf IdTCPClient_Normal (für Multi-Threading)
      }
      LVerbindungNormal: TRTLCriticalSection;

      {*
       Regelt den Zugriff auf IdTCPClient_Kamera (für Multi-Threading)
      }
      LVerbindungKamera: TRTLCriticalSection;

      {*
       Regelt den Zugriff auf befehle (für Multi-Threading)
      }
      LBefehle: TSRWLock;

      {*
       Regelt den Zugriff auf antworten (für Multi-Threading)
      }
      LAntworten: TSRWLock;

      {*
       Regelt den Zugriff auf kamerabild (für Multi-Threading)
      }
      LKamerabild: TSRWLock;

      {*
        Sendet Daten an den Controller und empfängt die Antwort.

        @param Nachricht Beinhaltet die Daten, die gesendet werden sollen.
        @param Antwort Beinhaltet die Daten, die empfangen wurden (wird in der Methode erzeugt).
        @param AntwortLaenge Die Anzahl Bytes, die vom Controller empfangen werden sollen.
        @return True, wenn beim Senden und Empfangen keine Fehler aufgetreten sind.
      }
      function Senden(Nachricht: TMemoryStream;
                      out Antwort: TMemoryStream;
                      AntwortLaenge: Int64)
            : Boolean; overload;

      {*
        Sendet Daten an den Controller und überprüft die Antwort.

        @param Nachricht Beinhaltet die Daten, die gesendet werden sollen.
        @param ErwarteteResponseID ID, mit der die Antwort des Controllers verglichen werden soll.
        @return True, wenn beim Senden und Empfangen keine Fehler aufgetreten sind
                und die empfangene ID gleich der erwarteten ist.
      }
      function Senden(Nachricht: TMemoryStream;
                      ErwarteteResponseID: ftIF2013ResponseId)
            : Boolean; overload;

      {*
        Sendet eine BefehlsID an den Controller und überprüft die Antwort.

        @param NachrichtID ID, die an den Controller gesendet werden soll.
        @param ErwarteteResponseID ID, mit der die Antwort des Controllers verglichen werden soll.
        @return True, wenn beim Senden und Empfangen keine Fehler aufgetreten sind
                und die empfangene ID gleich der erwarteten ist.
      }
      function Senden(NachrichtID: ftIF2013CommandId;
                      ErwarteteResponseID: ftIF2013ResponseId)
            : Boolean; overload;
  end;

implementation

{ TTXTController }

constructor TTXTController.Create(IP_Adresse: String);
begin
  inherited Create;

  InitializeCriticalSection(LVerbindungNormal);
  InitializeCriticalSection(LVerbindungKamera);
  LBefehle := TSRWLock.Create;
  LAntworten := TSRWLock.Create;
  LKamerabild := TSRWLock.Create;
  IdTCPClient_Normal := TIdTCPClient.Create;
  IdTCPClient_Kamera := TIdTCPClient.Create;

  letzter_Befehl := TTXTController_Steuerbefehl.Create(self);

  kamerabild := TJPEGImage.Create;

  IdTCPClient_Normal.Host := IP_Adresse;
  IdTCPClient_Normal.Port := 65000;
  IdTCPClient_Normal.ConnectTimeout := 4000;
  IdTCPClient_Normal.ReadTimeout := 4000;

  IdTCPClient_Kamera.Host := IP_Adresse;
  IdTCPClient_Kamera.Port := 65001;
  IdTCPClient_Kamera.ConnectTimeout := 4000;
  IdTCPClient_Kamera.ReadTimeout := 1000;
end;

destructor TTXTController.Destroy;
begin
  if IdTCPClient_Normal.Connected then
    Trennen;

  if IdTCPClient_Kamera.Connected then
    StoppeKamera;

  DeleteCriticalSection(LVerbindungNormal);
  DeleteCriticalSection(LVerbindungKamera);

  FreeAndNil(LBefehle);
  FreeAndNil(LAntworten);
  FreeAndNil(LKamerabild);

  FreeAndNil(IdTCPClient_Normal);
  FreeAndNil(IdTCPClient_Kamera);

  FreeAndNil(kamerabild);

  inherited;
end;

function TTXTController.EmpfangeBild: Boolean;
var
  Kommando: TMemoryStream;
  Antwort: TMemoryStream;
  Bild: TMemoryStream;
  BildData: KameraData;
  KommandoID: int32;
begin

  EnterCriticalSection(LVerbindungKamera);

  Kommando := TMemoryStream.Create;
  Antwort := TMemoryStream.Create;
  Bild := TMemoryStream.Create;

  Try

    try
      if not IdTCPClient_Kamera.IOHandler.InputBufferIsEmpty then
        CodeSite.SendWarning('EmpfangeBild: InputBuffer.Size: ' + IntToStr(IdTCPClient_Kamera.IOHandler.InputBuffer.Size));
      // Hängt manchmal hier (Controller sendet kein Bild)
      // Abbruch nach ReadTimeout
      IdTCPClient_Kamera.Socket.ReadStream(Antwort, sizeof(BildData));
    except
      on E: Exception do
      begin
        CodeSite.SendWarning('Kamerabildheader konnte nicht empfangen werden: ' + E.ToString);
        FreeAndNil(Kommando);
        FreeAndNil(Antwort);
        FreeAndNil(Bild);
        LeaveCriticalSection(LVerbindungKamera);
        CodeSite.Send('Starte Kamera neu...');
        StoppeKamera;
        StarteKamera;
        Exit(False);
      end;
    end;

    Antwort.Position := 0;
    Antwort.Read(BildData, sizeof(BildData));

    //CodeSite.Send('numFrameReady: ' + IntToStr(BildData.m_numframerady));

    Try
      IdTCPClient_Kamera.IOHandler.ReadStream(Bild, BildData.m_framesizecompressed);
      Bild.Position := 0;
      Result := True;
    Except
      On e: EIdReadTimeout do
      begin
        CodeSite.SendWarning('Kamerabild konnte nicht empfangen werden');
        Result := False;
      end;
    End;

    if Result then
    begin
      LKamerabild.AcquireExclusive;
      Try
        kamerabild.Free;
        kamerabild := TJPEGImage.Create;
        kamerabild.LoadFromStream(Bild);
      Finally
        LKamerabild.ReleaseExclusive;
      End;
    end;


    Try
      Kommando.Position := 0;
      KommandoID := int32(ftIF2013AcknowledgeId_CameraOnlineFrame);
      Kommando.Write(KommandoID, sizeof(KommandoID));
      Kommando.Position := 0;
      IdTCPClient_Kamera.IOHandler.Write(Kommando, Kommando.Size);
    Except
      CodeSite.SendError('Fehler beim Senden der Bestätigung des Kamerabildes');
      Result := False;
    End;

  Finally
    LeaveCriticalSection(LVerbindungKamera);
    FreeAndNil(Kommando);
    FreeAndNil(Antwort);
    FreeAndNil(Bild);
  End;

end;

function TTXTController.KamerabildBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  LKamerabild.AcquireShared;
  try
    Result.SetSize(kamerabild.Width, kamerabild.Height);
    kamerabild.Canvas.Lock;
    Result.Canvas.Draw(0, 0, kamerabild); // JPEG-Fehler #68
  finally
    kamerabild.Canvas.Unlock;
    LKamerabild.ReleaseShared;
  end;
end;

function TTXTController.KamerabildJpeg: TJpegImage;
begin
  Result := TJpegImage.Create;
  // TODO: Lock reicht nicht aus; aufrufende Funktion kann auf das zugrundeliegenede TJPEGData zugreifen
  // Bild vor dem zurückgeben kopieren?
  LKamerabild.AcquireShared;
  try
    Result.Assign(kamerabild);
  finally
    LKamerabild.ReleaseShared;
  end;
end;

function TTXTController.Konfiguration: TTXTController_Konfiguration;
begin
  Result := konfiguration_;
end;

function TTXTController.LiesAnalog(Eingangsnummer: Integer): Integer;
begin
  LAntworten.AcquireShared;
  try
    Result := antworten.m_universalInputs[Eingangsnummer - 1];
  finally
    LAntworten.ReleaseShared;
  end;
end;

function TTXTController.LiesCounter(Counternummer: Integer): Integer;
begin
  LAntworten.AcquireShared;
  try
    Result := antworten.m_counter_value[Counternummer - 1];
  finally
    LAntworten.ReleaseShared;
  end;
end;

function TTXTController.LiesDigital(Eingangsnummer: Integer): Boolean;
begin
  LAntworten.AcquireShared;
  try
    Result := not(antworten.m_universalInputs[Eingangsnummer - 1] = 0);
  finally
    LAntworten.ReleaseShared;
  end;
end;

function TTXTController.LiesIrSchalter(Kanal: Integer; Schalter: TFernbedienungsSchalter): Boolean;
var Daten: IR;
begin
  Lantworten.AcquireShared;
  Daten := antworten.m_ir[Kanal];
  Lantworten.ReleaseShared;

  case Schalter of
    AN: Result := (Daten.m_ir_bits and 1) <> 0;
    AUS: Result := (Daten.m_ir_bits and 2) <> 0;
    SCHALTER1: Result := (Daten.m_ir_bits and 4) <> 0;
    SCHALTER2: Result := (Daten.m_ir_bits and 8) <> 0;
    else assert(false);
  end;
end;

function TTXTController.LiesIrSteuerknueppelposition (Kanal: Integer;
    Seite: TSteuerknueppelSeite; Richtung: TSteuerknueppelRichtung): Integer;
var Daten: IR;
begin
  Lantworten.AcquireShared;
  Daten := antworten.m_ir[Kanal];
  Lantworten.ReleaseShared;
    if Seite = LINKS then
      if RICHTUNG = HORIZONTAL then
        Result := Daten.m_ir_leftX
      else
        Result := Daten.m_ir_leftY
    else
      if RICHTUNG = HORIZONTAL then
        Result := Daten.m_ir_rightX
      else
        Result := Daten.m_ir_rightY
end;

function TTXTController.LiesSensordaten: Boolean;
begin
  Result := Self.Steuerbefehl.Senden;
end;

function TTXTController.Senden(Nachricht: TMemoryStream;
  out Antwort: TMemoryStream; AntwortLaenge: Int64): Boolean;
begin

  //CodeSite.EnterMethod('TTXTController.Senden');

  EnterCriticalSection(LVerbindungNormal);

  if not IdTCPClient_Normal.Connected then
  begin
    CodeSite.SendError('Der TXT-Controller ist nicht verbunden!');
    Result := False;
    LeaveCriticalSection(LVerbindungNormal);
    // CodeSite.ExitMethod('TTXTController.Senden');
    Exit;
  end;

  try
    Nachricht.Position := 0;

    if not IdTCPClient_Normal.IOHandler.InputBufferIsEmpty then
      CodeSite.Send('Senden: Input Buffer[' + IntToStr(IdTCPClient_Normal.IOHandler.InputBuffer.Size) + '] :' + IdTCPClient_Normal.IOHandler.InputBufferAsString);
    IdTCPClient_Normal.IOHandler.Write(Nachricht, Nachricht.Size);

    Antwort := TMemoryStream.Create;
    IdTCPClient_Normal.IOHandler.ReadStream(Antwort, AntwortLaenge, False);

    Antwort.Position := 0;

    Result := True;
  except
    On e: Exception do
    begin
      CodeSite.SendError('Fehler in Funktion TTXTController.Senden1; Verbindung abgebrochen? ' + e.Message);
      Result := False;
      FreeAndNil(Antwort);
    end;
  end;

  LeaveCriticalSection(LVerbindungNormal);
  //CodeSite.ExitMethod('TTXTController.Senden');


end;

function TTXTController.Senden(Nachricht: TMemoryStream;
  ErwarteteResponseID: ftIF2013ResponseId): Boolean;
begin
  EnterCriticalSection(LVerbindungNormal);
  try

    if not IdTCPClient_Normal.Connected then
    begin
      CodeSite.SendError('Der TXT-Controller ist nicht verbunden!');
      Result := False;
      Exit;
    end;

    try
      Nachricht.Position := 0;

      if not IdTCPClient_Normal.IOHandler.InputBufferIsEmpty then // Log um Fehler bei den Paketgrößen zu erkennen
        CodeSite.Send('Input Buffer[' + IntToStr(IdTCPClient_Normal.IOHandler.InputBuffer.Size) + '] :' + IdTCPClient_Normal.IOHandler.InputBufferAsString);
      IdTCPClient_Normal.IOHandler.Write(Nachricht, Nachricht.Size);

      if not IdTCPClient_Normal.IOHandler.ReadInt32 = int32(ErwarteteResponseID) then
      begin
        CodeSite.SendError('Falsche ResponseID; ' + IntToHex(int32(ErwarteteResponseID), 8) + ' erwartet')
      end;

      Result := True;
    except
      On e: Exception do
      begin
        CodeSite.SendError('Fehler in Funktion TTXTController.Senden2; Verbindung abgebrochen? ' + e.Message);
        Result := False;
      end;
    end;

  finally
    LeaveCriticalSection(LVerbindungNormal);
  end;
end;

function TTXTController.Senden(NachrichtID: ftIF2013CommandId; ErwarteteResponseID: ftIF2013ResponseId): Boolean;
var Nachricht: TMemoryStream;
    ID: Int32;
begin
  Nachricht := TMemoryStream.Create;
  ID := int32(NachrichtID);
  Nachricht.Write(ID, SizeOf(ID));
  Result := Senden(Nachricht, ErwarteteResponseID);
  FreeAndNil(Nachricht);
end;

function TTXTController.StarteKamera: Boolean;
var
  StartCam: FTXStartCam;
  Kommando{, Antwort}: TMemoryStream;
//  ID: Int32;
begin

Senden(ftIF2013CommandId_StopCameraOnline, ftIF2013ResponseId_StopCameraOnline);
//Sleep(300);

// Aus dem C++-Beispielcode:
//
// Start camera server
// Tested resolutions/frame rates are
// 160 x 120 @ 60fps (useful for closed loop control applications)
// 320 x 240 @ 30fps
// 640 x 480 @ 15fps (might lead to frame drops / distortions, especially over WiFi/BT)
// Other resolutions might be supported, see "Resolutions.txt"
// Many resolutions which are supported by the camera overload the TXT,
// so there is no guarantee that any of these work!
// Also the ft-camera seems to have some bugs, e.g. 1280x960 result in 1280x720.
// More expensive cameras with large internal buffers might support higher resolutions.
//
// width         = requested frame width
// height        = requested frame height
// framerate     = requested frame rate in frames per second
// powerlinefreq = Frequency of artificial illumination
// This is required to adjust exposure to avoid flicker
// Supported values are 50 and 60

  // Grundeinstellungen
  StartCam.m_id := int32(ftIF2013CommandId_StartCameraOnline);

  // Tested resolutions
  // frame rates for the ft-camera are 320x240@30fps and 640x480@15fps
  StartCam.m_width := {160;//} 320;
  StartCam.m_height := {120;//} 240;
  StartCam.m_framerate := {60;//} 30;
  StartCam.m_powerlinefreq := 0; // 0=auto, 1=50Hz, 2=60Hz

  Kommando := TMemoryStream.Create;
  Kommando.Write(StartCam, sizeof(StartCam));
  //Result := Senden(Kommando, Antwort, sizeof(ID));

  Result := Senden(Kommando, ftIF2013ResponseId_StartCameraOnline);
  if Result = False then
  begin
    CodeSite.SendError('Fehler in StarteKamera, konnte StartCameraOnline nicht senden oder falsche Antwort');
    FreeAndNil(Kommando);
    Exit;
  end;

  // Antwort.Read(ID, sizeof(ID));

  //FreeAndNil(Antwort);

  FreeAndNil(Kommando);

  {if ID <> Int32(ftIF2013ResponseId_StartCameraOnline) then
  begin
    CodeSite.SendError('Falsche ID; ftIF2013ResponseId_StartCameraOnline erwartet');
    Result := False;
    Exit;
  end;   }

  Sleep(1000);

  Try
    EnterCriticalSection(LVerbindungKamera);

    if IdTCPClient_Kamera.Connected then
    begin
      Result := True;
      CodeSite.SendWarning('Kameraverbindung besteht bereits; Beende Kameraverbindung');
      IdTCPClient_Kamera.Disconnect;
      IdTCPClient_Kamera.IOHandler.InputBuffer.Clear;
    end;

    Try
      IdTCPClient_Kamera.Connect;      // Starte Kamera
      //CodeSite.Send('Kameraverbindung hergestellt');
    Except
      CodeSite.SendError('Verbindung für Kamera kann nicht hergestellt werden!');
      Result := False;
      Exit;
    End;
  Finally
    LeaveCriticalSection(LVerbindungKamera);
  End;
end;

function TTXTController.Steuerbefehl: TTXTController_Steuerbefehl;
begin
  Result := letzter_Befehl;
end;

function TTXTController.StoppeKamera: Boolean;
begin
  //CodeSite.EnterMethod('TTXTController.StoppeKamera');
  Result := Senden(ftIF2013CommandId_StopCameraOnline, ftIF2013ResponseId_StopCameraOnline);
  EnterCriticalSection(LVerbindungKamera);
  IdTCPClient_Kamera.Disconnect;
  IdTCPClient_Kamera.IOHandler.InputBuffer.Clear; //TODO: wird die Zeile gebraucht?
  LeaveCriticalSection(LVerbindungKamera);
  //CodeSite.ExitMethod('TTXTController.StoppeKamera');
end;

function TTXTController.Trennen: Boolean;
begin
  //CodeSite.EnterMethod('TTXTController.Trennen');
  Result := Senden(ftIF2013CommandId_StopOnline, ftIF2013ResponseId_StopOnline);
  EnterCriticalSection(LVerbindungNormal);
  IdTCPClient_Normal.Disconnect;
  IdTCPClient_Normal.IOHandler.InputBuffer.Clear; //TODO: wird die Zeile gebraucht?
  LeaveCriticalSection(LVerbindungNormal);
  //CodeSite.ExitMethod('TTXTController.Trennen');
end;

function TTXTController.HoleStatus: ftIF2013Response_QueryStatus;
var
  Kommando: TMemoryStream;
  Antwort: TMemoryStream;
  ID: Int32;
  status: ftIF2013Response_QueryStatus;
begin
  //CodeSite.EnterMethod('TTXTController.HoleStatus');
  Kommando := TMemoryStream.Create;

  Try
  ID := int32(ftIF2013CommandId_QueryStatus);
  Kommando.Write(ID, sizeof(ID));

  if Senden(Kommando, Antwort, sizeof(status))then
    Antwort.Read(status, sizeof(status));

  Finally
    FreeAndNil(Kommando);
    FreeAndNil(Antwort);
    //CodeSite.ExitMethod('TTXTController.HoleStatus');
  End;
end;

function TTXTController.IstVerbunden: Boolean;
begin
  Try
    Result := IdTCPClient_Normal.Connected;
  Except
    On e: Exception do
      Result := False;
  End;
end;

function TTXTController.Verbinden: Boolean;
var
  Kommando: TMemoryStream;
  name: array [0 .. 63] of AnsiChar;
  ID: Int32;
begin
  fillchar(name,sizeOf(name),0) ;
  name[0] := '4';
  name[1] := '2';

  Result := False;

  EnterCriticalSection(LVerbindungNormal);
  Try
    if IdTCPClient_Normal.Connected then
    begin
      CodeSite.SendWarning('Controller ist bereits verbunden, verbinde neu.');
      IdTCPClient_Normal.Disconnect;
      IdTCPClient_Normal.IOHandler.InputBuffer.Clear;
    end
    else
      Try
        IdTCPClient_Normal.Connect; // Verbinden
        Result := True;
      Except
        On e:Exception do
          begin
          CodeSite.SendError('Verbindung kann nicht hergestellt werden!' + System.sLineBreak +
                      'Ist der Controller eingeschaltet und in Reichweite?' + System.sLineBreak +
                      e.Message);
        end;
      End;
  Finally
    LeaveCriticalSection(LVerbindungNormal);
  End;

  if Result = True then
  begin
    ID := int32(ftIF2013CommandId_StartOnline);
    Kommando := TMemoryStream.Create;
    Kommando.Write(ID, sizeof(ID));
    Kommando.Write(name, sizeof(name));

    Result := Senden(Kommando, ftIF2013ResponseId_StartOnline);

    if not Result then
    begin
      CodeSite.SendError('ftIF2013ResponseId_StartOnline erwartet');
      Result := False;
      Exit;
    end;

    FreeAndNil(Kommando);
  end;
end;

{ TTXTController_Konfiguration }

constructor TTXTController_Konfiguration.Create(roboter: TTXTController);
begin
  self.roboter := roboter;
  FillChar(paket, sizeof(paket), #0);
  paket.m_id := int32(ftIF2013CommandId_UpdateConfig);
  lock := TSRWLock.Create;
end;

function TTXTController_Konfiguration.SetzeEingangsmodus
  (Eingangsnummer: Integer; Modus: TEingangsmodus): TTXTController_Konfiguration;
begin
  Inc(Eingangsnummer);

  lock.AcquireExclusive;
  case Modus of
    DIGITAL:
    begin
      paket.m_config.uni[Eingangsnummer].mode := Byte(InputMode.MODE_R);
      paket.m_config.uni[Eingangsnummer].digital := True;
    end;
    ANALOG:
    begin
      paket.m_config.uni[Eingangsnummer].mode := Byte(InputMode.MODE_R);
      paket.m_config.uni[Eingangsnummer].digital := False;
    end;
    SPANNUNG:
    begin
      paket.m_config.uni[Eingangsnummer].mode := Byte(InputMode.MODE_U);
      paket.m_config.uni[Eingangsnummer].digital := False;
    end;
    ENTFERNUNG:
    begin
      paket.m_config.uni[Eingangsnummer].mode := Byte(InputMode.MODE_ULTRASONIC);
      paket.m_config.uni[Eingangsnummer].digital := False;
    end;
  end;
  lock.ReleaseExclusive;

  Result := Self;
end;

function TTXTController_Konfiguration.SetzeAusgangsmodus
  (Motornummer: Integer; Modus: TAusgangsmodus): TTXTController_Konfiguration;
begin
  Inc(Motornummer);

  lock.AcquireExclusive;
  case Modus of
    MOTOR:
      paket.m_config.motor[Motornummer] := 0;
    ZWEI_AUSGAENGE:
      paket.m_config.motor[Motornummer] := 1;
  end;
  lock.ReleaseExclusive;

  Result := Self;
end;

function TTXTController_Konfiguration.Senden: Boolean;
var
  Kommando: TMemoryStream;
  Antwort: TMemoryStream;
  ID: Int32;
begin
  Kommando := TMemoryStream.Create;

  Kommando.Position := 0;
  lock.AcquireExclusive;
  Inc(paket.m_config_id);
  Kommando.Write(paket, sizeof(paket));
  lock.ReleaseExclusive;

  Result := roboter.Senden(Kommando, Antwort, sizeof(ID));
  Antwort.Read(ID, sizeof(ID));

  if ID <> Int32(ftIF2013ResponseId_UpdateConfig) then
  begin
    CodeSite.SendError('Falsche ID; ftIF2013ResponseId_UpdateConfig erwartet: '
                        + IntToHex(int32(ftIF2013ResponseId_UpdateConfig), 8));
    Result := False;
  end;

  FreeAndNil(Kommando);
  FreeAndNil(Antwort);
end;

{ TTXTController_Steuerbefehl }

constructor TTXTController_Steuerbefehl.Create(roboter: TTXTController);
begin
  self.roboter := roboter;
  FillChar(paket, sizeof(paket), #0);
  paket.m_id := int32(ftIF2013CommandId_ExchangeData);
  lock := TSRWLock.Create;
end;

function TTXTController_Steuerbefehl.Senden: Boolean;
var
  Kommando: TMemoryStream;
  Antwort: TMemoryStream;
  I: Integer;
begin
  Result := False;

  Kommando := TMemoryStream.Create;

  Kommando.Position := 0;
  lock.AcquireShared;
  Kommando.Write(paket, sizeof(paket));

  if roboter.Senden(Kommando, Antwort, sizeof(roboter.antworten)) then
  begin
    roboter.LAntworten.AcquireExclusive;
    Try
      Antwort.Read(roboter.antworten, sizeof(roboter.antworten));
    Finally
      roboter.LAntworten.ReleaseExclusive;
    End;

    Result := True;
    FreeAndNil(Antwort);
  end;

  for I := 0 to 3 do
  begin
    letzte_motor_id[I] := paket.m_motor_command_id[I];
  end;
  letzte_sound_id := paket.m_sound_command_id;
  lock.ReleaseShared;

  FreeAndNil(Kommando);
end;

function TTXTController_Steuerbefehl.SetzeAusgang(Ausgangsnummer,
  PWM_Wert: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_pwmOutputValues[Ausgangsnummer-1] := PWM_Wert;
  lock.ReleaseExclusive;

  Result := self;
end;

function TTXTController_Steuerbefehl.SetzeGeraeusch
  (Geraeusch, Wiederholungen: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_sound_index := Geraeusch;
  paket.m_sound_repeat := Wiederholungen;
  paket.m_sound_command_id := letzte_sound_id+1;
  lock.ReleaseExclusive;

  Result := self;
end;

function TTXTController_Steuerbefehl.SetzeMotorgeschwindigkeit
  (Motornummer, Geschwindigkeit: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_pwmOutputValues[Motornummer * 2 - 2] := Max(0, Geschwindigkeit);
  paket.m_pwmOutputValues[Motornummer * 2 - 1] := Max(0, -Geschwindigkeit);
  lock.ReleaseExclusive;

  Result := self;
end;

function TTXTController_Steuerbefehl.SetzeMotorschrittzahl(Motornummer,
  Schrittzahl: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_motor_distance[Motornummer-1] := Schrittzahl;
  paket.m_motor_command_id[Motornummer-1] := letzte_motor_id[Motornummer-1]+1;
  lock.ReleaseExclusive;

  Result := self;
end;

function TTXTController_Steuerbefehl.SetzeMotorsynchronisation(Motornummer1,
  Motornummer2: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_motor_master[Motornummer1-1] := Motornummer2;
  paket.m_motor_command_id[Motornummer1-1] := letzte_motor_id[Motornummer1-1]+1;
  lock.ReleaseExclusive;

  Result := self;
end;

function TTXTController_Steuerbefehl.HebeMotorsynchronisationAuf(
  Motornummer: Integer): TTXTController_Steuerbefehl;
begin
  lock.AcquireExclusive;
  paket.m_motor_master[Motornummer-1] := 0;
  paket.m_motor_command_id[Motornummer-1] := letzte_motor_id[Motornummer-1]+1;
  lock.ReleaseExclusive;

  Result := self;
end;

end.
