   import pandas as pd
from pathlib import Path
import conexion as conn
import cx_Oracle

def ora_conndb():
    cx_Oracle.init_oracle_client(lib_dir=r"C:/oracle/instantclient_21_13")
    server ="127.0.0.1" #input('Ingrese el IP del servidor de la base de datos :')
    database ="xe" #input('Ingrese el nombre de la base de datos:  ')
    username ="jardineria"  #input('Ingrese el usuario de la base de datos: ')
    password ="Admin12345"  #input('Ingrese el contraseña de la base de datos : ')
    #dsn = cx_Oracle.makedsn(server, '1521', service_name=database)
    #connection = cx_Oracle.connect(user=username, password=password, dsn=dsn)
    connection =cx_Oracle.connect(user="jardineria", password="Admin12345",dsn="localhost/xe")
    return connection

oradbconn = ora_conndb()

# Seccion de utilidades para el manejos de dataframes
# Función para revisar las longitudes de los valores en cada columna en un dataframe

def check_lengths(dataframe, max_length):
    # uso check_lengths(df, max_length)
    pd.set_option('display.max_rows', None)
    pd.set_option('display.max_columns', None)
    for column in dataframe.columns:
        if dataframe[column].apply(lambda x: len(str(x)) > max_length).any():
            print(f"Columna: {column} contiene valores demasiado largos:")
            print(dataframe[dataframe[column].apply(lambda x: len(str(x)) > max_length)])
            print("\n")
   
def max_column_sizes(df):
    #pd.set_option('display.max_rows', None)
    #pd.set_option('display.max_columns', None)
    #pd.set_option('display.width', None)
    column_sizes = df.applymap(lambda x: len(str(x))).max()
    sorted_sizes = column_sizes.sort_values(ascending=False)
    sorted_sizes = max_column_sizes(df)
    print(sorted_sizes)
    
#Seccion de utilidades de base de datos 
def create_sql_insert(owner,table,numfields):
    sql = "INSERT INTO {0}.{1} VALUES (".format(owner,table)
    for i in range(1, numfields + 1):
        if i == numfields:
            sql += f":{i})"
        else:
            sql += f":{i}, "
    return sql

def count_columns(table):
    cursor = oradbconn.cursor()
    select_query = f"SELECT COUNT(*) FROM user_tab_columns WHERE table_name = '{table.upper()}'"
    cursor.execute(select_query)
    result = cursor.fetchone()[0]
    cursor.close()
    return result

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

def table_atributes(table_name,opcion):
    result=None
    cursor = oradbconn.cursor()
    if opcion==1:
        cursor.execute(f"SELECT COUNT(*) FROM user_tab_columns WHERE table_name = '{table_name}'")
        result = cursor.fetchone()[0]
    if opcion==2:
        cursor.execute(f"SELECT column_name FROM user_tab_columns WHERE table_name = '{table_name}'")
        result = [row[0] for row in cursor.fetchall()]
    cursor.close()
    return result

def statement_insert(table_name):
    column_count=table_atributes(table_name,1)
    column_names=table_atributes(table_name,2)
    insert_query = f"INSERT INTO {table_name} ({', '.join(column_names)}) VALUES ({', '.join(['?'] * column_count)})"
    return insert_query

def get_oracle_data(owner,table_name):
    cursor = oradbconn.cursor()
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

def execute_oracle_procedure(pprocedure):
    cursor = oradbconn.cursor()
    cursor.callproc(pprocedure)
