

(*A permission is a triple Set, Entity, List of Allowed Actions
Set is the set of the elements providing the domain of interest (e.g. File, Network...)
Entity is the name of an element of the domain (if '*' is used then the permission is
for all the elements in the domain)
Allowed Actions is the list of allowed actions for a given element of the domain*)

type permission = Permission of string * string * string list
type pdomain = permission list 

(*A Security Action (secAction) is a operation acting on permissions (e.g
grant of a pdomain or enable/disable a permission)*)

type secAction = 
  | Grant of pdomain
  | Disable of permission
  | Enable of permission

type pstack = secAction list

let rec sublist l1 l2 = 
  match l1 with
  | [] -> true
  | e :: l -> if List.mem e l2 then sublist l l2 else false

(* allows controlla che la request sia coerente con i permessi dell'entità
a cui vogliamo accedere. Prima si controlla se i domini solo gli stessi(s1=s2)
poi se le entità sono le stesse o r1=* e infine se la lista di azioni che 
vogliamo eseguire su a1 ; cioè a2, siano contenute nella lista di permessi, a1*)

let allows (p:permission) (request:permission) = 
  let (Permission(s1,r1,a1)) = p in
  let (Permission(s2,r2,a2)) = request in
  s1 = s2 && (r1 = "*" || r1 = r2) && sublist a2 a1

(*
  domainInspection prende un pdomain, cioè una lista di (dominio,entità,permessi)
e una richiesta e scorre tutti gli elementi di pdomain finchè non ha 
controllato che tutte le operazioni, request, che vogliamo fare facciano parte
del pdomain, richiamando domainInspection. PASSA IN RASSEGNA tutti

GLI ELEMENTI DI PDOMAIN FINCHÈ UNA ~EGUAGLIANZA NON OCCORRE*)
  
let rec domainInspection (set:pdomain) (request:permission) = 
  match set with
  | [] -> 0 
  | p :: s -> if allows p request then 1
                else domainInspection s request

let stackInspection inspectFun (stack:pstack) (request:permission) = 
  match stack with
  | [] -> 0
  | e :: l -> inspectFun (e::l) request

let rec inspectFun (stack:pstack) (request:permission) = 
  match stack with
  | [] -> 1
  | sa::sl -> (
    match sa with
    (* Grant serve per verificare il permesso *)
    | Grant domain -> 
      if domainInspection domain request = 1 then 1 else stackInspection inspectFun sl request 
    (* Enable serve per abilitare un permesso *)
    | Enable p -> 
      if allows p request then 1 else stackInspection inspectFun sl request
    (* Disable serve per disabilitare un permesso *)
    | Disable p -> 
      if allows p request then 0 else stackInspection inspectFun sl request)



