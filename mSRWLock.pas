{*
Dieses Modul enthält die Klasse TSRWLock.

Autor: Sven Stegemann
Datum: 06.01.2016

Copyright © 2015 FH Münster
                 Fachbereich Maschinenbau
                 Labor für Systemanalyse und Optimierung
}

unit mSRWLock;

interface

type
  /// Wird benutzt um Fehler beim gleichzeitigen Zugriff auf eine Resource zu verhindern.
  /// https://msdn.microsoft.com/de-de/library/windows/desktop/aa904937%28v=vs.85%29.aspx
  TSRWLock = class(TObject)
  private
    Lock: Pointer;
  public
    constructor Create;
    procedure AcquireShared;
    procedure ReleaseShared;
    procedure AcquireExclusive;
    procedure ReleaseExclusive;
  end;

implementation

procedure InitializeSRWLock(var P: Pointer); stdcall;
  external 'kernel32.dll';

procedure AcquireSRWLockShared(var P: Pointer); stdcall;
  external 'kernel32.dll';
procedure ReleaseSRWLockShared(var P: Pointer); stdcall;
  external 'kernel32.dll';

procedure AcquireSRWLockExclusive(var P: Pointer); stdcall;
  external 'kernel32.dll';
procedure ReleaseSRWLockExclusive(var P: Pointer); stdcall;
  external 'kernel32.dll';

constructor TSRWLock.Create;
begin
  InitializeSRWLock(Lock);
end;

procedure TSRWLock.AcquireShared;
begin
  AcquireSRWLockShared(Lock);
end;

procedure TSRWLock.ReleaseShared;
begin
  ReleaseSRWLockShared(Lock);
end;

procedure TSRWLock.AcquireExclusive;
begin
  AcquireSRWLockExclusive(Lock);
end;

procedure TSRWLock.ReleaseExclusive;
begin
  ReleaseSRWLockExclusive(Lock);
end;

{ Alternativ:

  unit mSRWLock;

  interface

  type
  TSRWLock = Pointer

  implementation

  procedure AcquireSRWLockShared(var P: TSRWLock); stdcall; external 'kernel32.dll';
  procedure ReleaseSRWLockShared(var P: TSRWLock); stdcall; external 'kernel32.dll';

  procedure AcquireSRWLockExclusive(var P: TSRWLock); stdcall; external 'kernel32.dll';
  procedure ReleaseSRWLockExclusive(var P: TSRWLock); stdcall; external 'kernel32.dll';

  end. }


end.
