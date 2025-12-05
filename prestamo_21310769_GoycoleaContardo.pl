% Exportar predicados
:- module(prestamo_21310769_GoycoleaContardo, [
    crearPrestamo/6,
    getPrestamoId/2,
    getPrestamoIdUsuario/2,
    getPrestamoIdLibro/2,
    getPrestamoFechaPrestamo/2,
    getPrestamoDiasSolicitados/2,
    isPrestamoActivo/1,
    setPrestamoActivo/3,
    obtenerFechaVencimiento/2,
    extraerDatosFecha/3,
    formatearFecha/3
]).

%=============== Constructor ===================

% RF04: crearPrestamo/6
% Descripción: Crea un préstamo. Los días solicitados no deben exceder el máximo permitido por el sistema. Formato fecha: "DD/MM" donde todos los meses tienen 30 días.
% Parametros: crearPrestamo(+Id, +IdUsuario, +IdLibro, +FechaPrestamo, +DiasSolicitados, -Prestamo)
% Algoritmo: fuerza bruta
crearPrestamo(Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, Prestamo) :-
    % Verificaciones
    integer(Id),
    integer(IdUsuario),
    integer(IdLibro),
    integer(DiasSolicitados),
    DiasSolicitados > 0,
    (string(FechaPrestamo) ; atom(FechaPrestamo)),
    % Prestamo = [Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, Activo=true]
    Prestamo = [Id, IdUsuario, IdLibro, FechaPrestamo, DiasSolicitados, true].

%=============== Funciones de pertenencia ===================

% esPrestamo/1
% Descripción: Verifica si elemento es un prestamo valido
% Parametros: esPrestamo(+SupuestoPrestamo) -Booleano
% Algoritmo: fuerza bruta
esPrestamo([Id, IdUsuario, IdLibro, Fecha, Dias, Activo]) :-
    integer(Id),
    integer(IdUsuario),
    integer(IdLibro),
    (atom(Fecha) ; string(Fecha)),
    integer(Dias),
    Dias > 0,
    (Activo = true ; Activo = false).

%=============== Selectores ===================

% getPrestamo{Elemento}/2
% Descripción: Obtiene el {Elemento} del prestamo
% Parametros: getPrestamo{Elemento}(+Prestamo, -Elemento)
% Algoritmo: fuerza bruta
getPrestamoId([Id, _, _, _, _, _], Id).
getPrestamoIdUsuario([_, IdUsuario, _, _, _, _], IdUsuario).
getPrestamoIdLibro([_, _, IdLibro, _, _, _], IdLibro).
getPrestamoFechaPrestamo([_, _, _, Fecha, _, _], Fecha).
getPrestamoDiasSolicitados([_, _, _, _, Dias, _], Dias).
isPrestamoActivo([_, _, _, _, _, true]).

%=============== Modificadores ===================

% setPrestamoActivo/3
% Descripción: Modifica el estado activo del préstamo
% Parametros: prestamo(+Prestamo, +Activo, -PrestamoModificado)
% Algoritmo: fuerza bruta
setPrestamoActivo([Id, IdUsuario, IdLibro, Fecha, Dias, _], NuevoActivo, [Id, IdUsuario, IdLibro, Fecha, Dias, NuevoActivo]) :-
    (NuevoActivo = true ; NuevoActivo = false).

%=============== Otros ===================

% RF15: obtenerFechaVencimiento/2
% Descripción: Calcula fecha de vencimiento sumando días solicitados a fecha de préstamo. Formato "DD/MM", todos los meses tienen 30 días. Si pasa del día 30, cambia de mes.
% Parametros: obtenerFechaVencimiento(+Prestamo, -Fecha)
% Algoritmo: fuerza bruta
obtenerFechaVencimiento(Prestamo, FechaVencimiento) :-
    getPrestamoFechaPrestamo(Prestamo, FechaPrestamo),
    getPrestamoDiasSolicitados(Prestamo, DiasSolicitados),
    calcularFechaVencimiento(FechaPrestamo, DiasSolicitados, FechaVencimiento).

% calcularFechaVencimiento/3
% Descripción: Auxicial a obtenerFechaVencimiento
% Parametros: calcularFechaVencimiento(+FechaInicial, +DiasAgregar, -FechaFinal)
% Algoritmo: fuerza bruta
calcularFechaVencimiento(FechaInicial, DiasAgregar, FechaFinal) :-
    % Extraer datos fecha
    extraerDatosFecha(FechaInicial, DiaIni, MesIni),
    % Calcular nueva fecha
    NuevoDia is DiaIni + DiasAgregar,
    calcularFechaFinal(NuevoDia, MesIni, DiaFinal, MesFinal),
    % Formatear fecha final
    formatearFecha(DiaFinal, MesFinal, FechaFinal).

% extraerDatosFecha/3
% Descripción: Extrae dia y mes de una fecha
% Parametros: extraerDatosFecha(+Fecha, -Dia, -Mes)
% Algoritmo: fuerza bruta
extraerDatosFecha(Fecha, Dia, Mes) :-
    atom_codes(Fecha, Codes),
    append(CodigosDia, [47|CodigosMes], Codes), % 47 es el código ASCII de '/'
    length(CodigosDia, 2), % Asegurar que el día tiene 2 dígitos
    number_codes(Dia, CodigosDia),
    number_codes(Mes, CodigosMes).

% calcularFechaFinal/4
% Descripción: Calcula dia y mes final pasando dias a meses y dejando el resto de dias que no forman un mes
% Parametros: calcularFechaFinal(+DiaTotal, +MesInicial, -DiaFinal, -MesFinal)
% Algoritmo: fuerza bruta

% En caso de que los dias no forman un mes retornar tal cual
calcularFechaFinal(DiaTotal, MesInicial, DiaFinal, MesFinal) :-
    DiaTotal =< 30,
    DiaFinal = DiaTotal,
    MesFinal = MesInicial.

% En caso de que los superan un mes (dias>30) hacer división entera para convertir a mes y dejar el resto de dias
calcularFechaFinal(DiaTotal, MesInicial, DiaFinal, MesFinal) :-
    DiaTotal > 30,
    MesesAgregar is (DiaTotal - 1) // 30,
    DiaFinal is ((DiaTotal - 1) mod 30) + 1,
    MesFinal is MesInicial + MesesAgregar.

% formatearFecha/3
% Descripción: Convierte dia y mes a formato fecha "DD/MM"
% Parametros: formatearFecha(+Dia, +Mes, -FechaFormateada)
% Algoritmo: fuerza bruta
formatearFecha(Dia, Mes, FechaFormateada) :-
    format(atom(DiaAtom), '~|~`0t~d~2+', [Dia]),
    format(atom(MesAtom), '~|~`0t~d~2+', [Mes]),
    atom_concat(DiaAtom, '/', Temp),
    atom_concat(Temp, MesAtom, FechaFormateada).
