select count(1), pais 
from v_cliente
group by pais;

use dw;

insert into dim_cliente(codigo_cliente, nombre_cliente, asesor_ventas, ciudad, region, pais, categoria)
select codigo_cliente, nombre_cliente, asesor_ventas, ciudad, region, pais, categoria from staging.dbo.v_cliente;
select * from dim_cliente
----Dimensiones producto provedor

-- Tabal de hechos
--idcliente,id producto, id_empleado , fecha, cantidad, precio

SELECT
    DC.id_cliente idcliente,
    NULL idproducto,
    NULL id_empleado,
    CAST(P.FECHA_PEDIDO AS DATE) fecha,
    DP.cantidad,
    DP.PRECIO_UNIDAD precio
FROM STAGING.DBO.PEDIDO P
INNER JOIN STAGING.DBO.DETALLE_PEDIDO DP
    ON P.CODIGO_PEDIDO = DP.CODIGO_PEDIDO
INNER JOIN dim_cliente DC
    ON CAST(P.CODIGO_CLIENTE AS VARCHAR(10)) = DC.codigo_cliente
	WHERE FECHA_ENTREGA IS NOT NULL


ALTER TABLE FACT_VENTAS ADD CONSTRAINT FK_VENTAS_CLIENTE FOREIGN KEY (IDCLIENTE) REFERENCES DIM_CLIENTE(ID_CLIENTE)

