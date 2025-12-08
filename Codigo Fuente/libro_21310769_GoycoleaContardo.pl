% Exportar predicados
:- module(libro_21310769_GoycoleaContardo, [
    crearLibro/4,
    getLibroId/2,
    getLibroTitulo/2,
    getLibroAutor/2,
    isLibroDisponible/1,
    setLibroDisponible/3,
    buscarLibroPorId/3,
    buscarLibroPorTitulo/3,
    buscarLibroPorAutor/3
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

%=============== Funciones de pertenencia ===================

% esLibro/1
% Descripción: Verifica si elemento es un libro valido
% Parametros: esLibro(+SupuestoLibro) -Booleano
esLibro([Id, Titulo, Autor, Disponible]) :-
    integer(Id),
    (atom(Titulo) ; string(Titulo)),
    (atom(Autor) ; string(Autor)),
    (Disponible = true ; Disponible = false).

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

%=============== Modificadores ===================

% setLibroDisponible/3
% Descripción: Settea disponibilidad del libro
% Parametros: setLibroDisponible(+Libro, +Disponible, -LibroModificado)
% Algoritmo: fuerza bruta
setLibroDisponible([Id, Titulo, Autor, _], NuevoDisponible, [Id, Titulo, Autor, NuevoDisponible]) :-
    (NuevoDisponible = true ; NuevoDisponible = false).

%=============== Buscadores ===================

% buscarLibroPorId/3
% Descripción: Buscar libro en una lista de libros por ID
% Parametros: buscarLibroPorId(+ListaLibros, +IdBuscado, -Libro)
% Algoritmo: fuerza bruta
buscarLibroPorId(ListaLibros, IdBuscado, Libro) :-
    member(Libro, ListaLibros),
    getLibroId(Libro, IdBuscado).

% buscarLibroPorTitulo/3
% Descripción: Buscar libro que su titulo contenga string buscada
% Parametros: buscarLibroPorTitulo(+ListaLibros, +TituloParcial, -Libro)
% Algoritmo: fuerza bruta
buscarLibroPorTitulo(ListaLibros, TituloParcial, Libro) :-
    % Convertir a minusculas
    downcase_atom(TituloParcial, TituloLower),
    member(Libro, ListaLibros),
    getLibroTitulo(Libro, TituloLibro),
    % Verificar si string esta contenida en el titulo
    sub_atom(TituloLibro, _, _, _, TituloLower).

% buscarLibroPorAutor/3
% Descripción: Buscar libro que su autor contenga string buscada
% Parametros: buscarLibroPorAutor(+ListaLibros, +AutorParcial, -Libro)
% Algoritmo: fuerza bruta
buscarLibroPorAutor(ListaLibros, AutorParcial, Libro) :-
    % Convertir a minusculas
    downcase_atom(AutorParcial, AutorLower),
    member(Libro, ListaLibros),
    getLibroAutor(Libro, AutorLibro),
    % Verificar si string esta contenida en el autor
    sub_atom(AutorLibro, _, _, _, AutorLower).
