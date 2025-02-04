import pyodbc
from sqlalchemy import create_engine
import cx_Oracle

cx_Oracle.init_oracle_client(lib_dir=r"C:/oracle/instantclient_21_13")

def ora_conndb():
    server = "localhost/xe"
    username = 'jardineria'
    pwd = 'Admin12345'

    connection = cx_Oracle.connect(
        user=username, password=pwd, dsn=server
    )
    return connection

def mssql_conndb(method=1):
    server =input('ingrese el nmombre del servidor ')
    database = 'STAGING'
    username = 'sa'
    password = 'Admin12345'

    if method == 1:
        conexion_str = (
            'DRIVER={SQL Server};SERVER=' + server + ';DATABASE=' + database +
            ';UID=' + username + ';PWD=' + password
        )
        conexion = pyodbc.connect(conexion_str)

    if method == 2:
        DATABASE_URL = (
            f'mssql+pyodbc://{username}:{password}@{server}/{database}'
            '?driver=ODBC+Driver+17+for+SQL+Server'
        )
        conexion = create_engine(DATABASE_URL)

    return conexion

source_conn = ora_conndb()
target_conn = mssql_conndb(1)

def get_user_tables():
    connection = ora_conndb()
    cursor = connection.cursor()
    try:
        cursor.execute("SELECT table_name FROM user_tables")
        tables = [row[0] for row in cursor.fetchall()]
        return tables
    except Exception as e:
        print(f"Error al obtener las tablas de usuario: {str(e)}")
        return None
    finally:
        cursor.close()
        connection.close()

def get_oracle_data(owner, table_name):
    cursor = source_conn.cursor()
    try:
        sql = f"SELECT * FROM {owner}.{table_name}"
        cursor.execute(sql)
        data = cursor.fetchall()
        return data
    except Exception as e:
        print(f"Error al obtener datos de Oracle: {str(e)}")
        return None
    finally:
        cursor.close()

def count_columns(table):
    cursor = source_conn.cursor()
    select_query = f"SELECT COUNT(*) FROM user_tab_columns WHERE table_name = '{table.upper()}'"
    cursor.execute(select_query)
    result = cursor.fetchone()[0]
    cursor.close()
    return result

def create_sql_insert(owner, table, numfields):
    # antes de insertar deben estar la tabla creada en SQL server 
    # SELECT DBMS_METADATA.GET_DDL('TABLE', 'CLIENTE') FROM DUAL;
    sql = f"INSERT INTO {owner}.{table} VALUES ({', '.join(['?' for _ in range(numfields)])})"
    return sql


def add_entity(owner,tablename):
    numfields = count_columns(tablename)
    data = get_oracle_data(owner, tablename)
    sql = create_sql_insert("dbo", tablename, numfields)

    if data:
        target_cursor = target_conn.cursor()
        target_cursor.executemany(sql, data)
        target_conn.commit()
        target_cursor.close()

def delete_data(table, operation, connection):
    cursor = connection.cursor()
    try:
        if operation.upper() == 'DELETE':
            delete_query = f"DELETE FROM {table}"
            cursor.execute(delete_query)
        elif operation.upper() == 'TRUNCATE':
            truncate_query = f"TRUNCATE TABLE {table}"
            cursor.execute(truncate_query)
        else:
            raise ValueError("Operación no válida. Use 'DELETE' o 'TRUNCATE'.")
        
        connection.commit()
    except Exception as e:
        print(f"Error al realizar la operación: {str(e)}")
    finally:
        cursor.close()

if __name__ == "__main__":
    owner='jardineria'
    user_tables = ['CLIENTE'] #  get_user_tables()
    if user_tables:
        print("Transfiriendo informacion:")
        for table in user_tables:
            delete_data(table, 'DELETE', target_conn)
            add_entity(owner,table)
            print ("Transferencia de "+str(table) +" Completa")
    else:
        print("No se pudieron obtener las tablas de usuario.")
    source_conn.close()
    target_conn.close()
    print("Inserción de datos SQL Server completada.")
