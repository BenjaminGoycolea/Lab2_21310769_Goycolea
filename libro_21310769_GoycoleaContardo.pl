% Exportar predicados
:- module(libro_21310769_GoycoleaContardo, [
    crearLibro/4,
    getLibroId/2,
    getLibroTitulo/2,
    getLibroAutor/2,

]).

%=============== Constructor ===================

% RF03: crearLibro/4
% Descripción: Crea un libro con id, título y autor. Título y autor se convierten en minúsculas al crear.
% Parametros: crearLibro(+Id, +Titulo, +Autor, -Libro)
% Algoritmo: fuerza bruta
crearLibro(Id, Titulo, Autor, Libro) :-
    % Verificaciones
    integer(Id),
    (string(Titulo) ; atom(Titulo)),
    (string(Autor) ; atom(Autor)),
    % Convertir titulo y autor a minusculas
    downcase_atom(Titulo, TituloLower),
    downcase_atom(Autor, AutorLower),
    % Libro = [Id, Titulo, Autor, Disponible=true]
    Libro = [Id, TituloLower, AutorLower, true].

%=============== Selectores ===================

% RF10: getLibroId/2
% Descripción: Obtiene el ID de un libro.
% Parametros: getLibroId(+Libro, -Id)
% Algoritmo: fuerza bruta
getLibroId([Id, _, _, _], Id).

% getLibro{Elemento}/2
% Descripción: Obtiene el {Elemento} del libro
% Parametros: getLibro{Elemento}(+Libro, -{Elemento})
% Algoritmo: fuerza bruta
getLibroTitulo([_, Titulo, _, _], Titulo).
getLibroAutor([_, _, Autor, _], Autor).
isLibroDisponible([_, _, _, true]).
