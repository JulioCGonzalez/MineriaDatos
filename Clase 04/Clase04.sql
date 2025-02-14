use dw;

create table dim_cliente( 
id_cliente int identity constraint pk_cliente primary key,
codigo_cliente nvarchar(30),
nombre_cliente nvarchar(80),
asesor_ventas nvarchar(80),
ciudad nvarchar(50),
region nvarchar(50),
pais nvarchar(50),
categoria nvarchar(50)
);

use staging;

alter view v_cliente as

with segmentos_cliente as (
select cast(codigo_cliente as varchar(10)) codigo_cliente, sum(dp.cantidad*dp.precio_unidad) ventas,
ntile(4) over (order by sum (dp.cantidad*dp.precio_unidad) desc) segmento
from pedido p
inner join detalle_pedido dp
on p.codigo_pedido=dp.codigo_pedido
where estado='ENTREGADO'
group by codigo_cliente

union 

select customerID, sum(dp.quantity*dp.unitprice) ventas,
ntile(4) over (order by sum(dp.quantity*dp.unitprice) desc) segmento
from orders p
inner join orderdetails dp
on p.orderid=dp.orderid
group by customerID
)

select 
 cp.codigo_cliente, 
 upper(nombre_cliente) nombre_cliente,
 upper(isnull(asesor_ventas, 'Lorena Paxton')) asesor_ventas,
 upper(ciudad) ciudad,
 upper(isnull(region, ciudad)) region,
 upper (case when pais= 'UK' then 'UNITED KINGDOM' else pais end) pais,
 case when isnull (segmento,0) = 1 then 'CLASE A' 
 when isnull (segmento,0) = 2 then 'CLASE B'
 when isnull (segmento,0) = 3 then 'CLASE C'
 when isnull (segmento,0) = 4 then 'CLASE D'
 else 'CLASE E' end
 categoria
from (

select cast(cj.CODIGO_CLIENTE as varchar(10)) codigo_cliente, cj.nombre_cliente, 
       CONCAT(ej.nombre, ' ', APELLIDO1, ' ', APELLIDO2) asesor_ventas,
       cj.ciudad, cj.region, cj.pais
from cliente cj
left join empleado ej
on cj.CODIGO_EMPLEADO_REP_VENTAS = ej.CODIGO_EMPLEADO

union all

select cn.CustomerID, cn.CompanyName, 
       CONCAT(en.FirstName,' ', en.LastName) asesor,
       cn.City, cn.Region, cn.Country
from Customers cn

left join (
select CustomerID, EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID)
group by CustomerID, EmployeeID
) ae
on cn.CustomerID = ae.CustomerID
left join Employees en
on en.EmployeeID=ae.EmployeeID
) cp 
left join segmentos_cliente sc
on cp.codigo_cliente = sc.codigo_cliente

----------------------------------------------

select * from v_cliente;


select CustomerID, EmployeeID from Orders o1
where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID)
group by CustomerID, EmployeeID


-- Con esto busco duplicados (si hay duplicados hay un problema)
--select count(*), CustomerID, EmployeeID from Orders o1
--where orderid=(select max(orderid) from Orders o2 where o2.CustomerID=o1.CustomerID)
--group by CustomerID, EmployeeID
--having count (1)>1


-------------------------------------------------------------------
-- ntile genera percentales (grupos que quiera)
select cast(codigo_cliente as varchar(10)) cliente, sum(dp.cantidad*dp.precio_unidad) ventas,
ntile(4) over (order by sum (dp.cantidad*dp.precio_unidad) desc) segmento
from pedido p
inner join detalle_pedido dp
on p.codigo_pedido=dp.codigo_pedido
where estado='ENTREGADO'
group by codigo_cliente

union 

select customerID, sum(dp.quantity*dp.unitprice) ventas,
ntile(4) over (order by sum(dp.quantity*dp.unitprice) desc) segmento
from orders p
inner join orderdetails dp
on p.orderid=dp.orderid
group by customerID;