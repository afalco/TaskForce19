import pandas as pd
from datetime import datetime, timedelta

spain_c19_casos_url = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv'
spain_c19_muertes_url = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv'
spain_c19_uci_url = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci_long.csv'
spain_c19_altas_url = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas_long.csv'
spain_c19_hosp_url = 'https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados_long.csv'
usa_c19_url = 'http://covidtracking.com/api/states/daily.csv'
world_pop_url = 'https://raw.githubusercontent.com/datasets/population/master/data/population.csv'
usa_states_pop = 'https://raw.githubusercontent.com/CivilServiceUSA/us-states/master/data/states.csv'


if __name__ == '__main__':
    df_spain_cases = pd.read_csv(spain_c19_casos_url)
    df_spain_deceased = pd.read_csv(spain_c19_muertes_url)

    ########################
    # CV
    ########################
    df_cv = df_spain_cases.loc[df_spain_cases['cod_ine'] == 10]
    df_cv = df_cv.rename(columns={'total': 'cases', 'fecha': 'dateRep'})
    df_cv = df_cv.drop(columns=['cod_ine', 'CCAA'])

    # adds data from 01-01 to 26-2, with num. cases = 0
    prev_dates = [str(datetime.strptime('2020-01-01', '%Y-%m-%d') + timedelta(days=d))[:10] for d in range(57)]
    df_prev_cv = pd.DataFrame(prev_dates, columns=['dateRep'])
    df_prev_cv['cases'] = 0
    df_cv = pd.concat((df_prev_cv, df_cv))

    df_cv['deaths'] = 0

    for i, row in df_spain_deceased.loc[df_spain_deceased['cod_ine'] == 10].iterrows():
        date = row['fecha']
        deaths = row['total']
        mask = df_cv['dateRep'] == date
        cases = df_cv.loc[mask, 'cases']
        df_cv.loc[mask, 'deaths'] = deaths

    df_cv['dateRep'] = df_cv['dateRep'].apply(lambda x: x.replace('-', '/'))
    df_cv['year'] = df_cv['dateRep'].apply(lambda x: f'{int(x[:4]):02d}')
    df_cv['month'] = df_cv['dateRep'].apply(lambda x: f'{int(x[5:7]):02d}')
    df_cv['day'] = df_cv['dateRep'].apply(lambda x: f'{int(x[8:]):02d}')

    df_cv['countriesAndTerritories'] = 'Comunitat_Valenciana'
    df_cv['geoId'] = 'CV'
    df_cv['countryterritoryCode'] = 'CVL'
    df_cv['popData2018'] = 5003769

    df_cv = df_cv[['dateRep', 'day', 'month', 'year', 'cases', 'deaths', 'countriesAndTerritories', 'geoId', 'countryterritoryCode', 'popData2018']]
    df_cv.to_csv('data/cv_covid19.csv', index=False)
