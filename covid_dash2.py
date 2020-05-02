import pandas as pd
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import plotly.express as px
import plotly.graph_objs as go
import numpy as np
from dash.exceptions import PreventUpdate

app = dash.Dash()

covid_data = pd.read_csv("data/covid_country_level_data.csv")
covid_data.drop(columns = ['s1_is_general', 's2_is_general', 's3_is_general', 's4_is_general',
                           's5_is_general', 's6_is_general', 'stringency_index', 'age_percent_0_to_14',
                           'age_percent_15_to_64'], inplace = True)

app.layout = html.Div([
    html.H1(id='title'),
    html.Div([
        dcc.Graph(id='map')],
        style={'width':'50%', 'display':'inline-block'}
    ),
    html.Div([
        dcc.Graph(id = 'growth')],
        style={'width':'50%', 'display':'inline-block'}
    ),
    html.Div([
        dcc.Dropdown(id="variable_selector",
            options = [{'label': 'School Closing', 'value': 's1_school_closing'},
                        {'label': 'Workplace Closing', 'value': 's2_workplace_closing'},
                        {'label': 'Public Events Cancelled', 'value': 's3_cancel_public_events'},
                        {'label': 'Public Transportation Closed', 'value': 's4_close_public_transport'},
                        {'label': 'Public Information Campaigns', 'value': 's5_public_information_campaigns'},
                        {'label': 'Restrictions on Internal Movement', 'value': 's6_restrictions_on_internal_movement'},
                        {'label': 'International Travel Controls', 'value': 's7_international_travel_controls'},
                        {'label': 'Fiscal Measures Emplaced', 'value': 's8_fiscal_measures'},
                        {'label': 'Monetary Measures Emplaced', 'value': 's9_monetary_measures'},
                        {'label': 'Emergency Investment in Health Care', 'value': 's10_emergency_investment_in_health_care'},
                        {'label': 'Investment in Vaccines', 'value': 's11_investment_in_vaccines'},
                        {'label': 'Contract Tracing', 'value': 's13_contact_tracing'},
                        {'label': 'Stringency Index', 'value': 'stringency_index_for_display'},
                        {'label': 'Hospital Beds per Million', 'value': 'hospital_beds_per_million'},
                        {'label': 'Population Density', 'value': 'people_per_sq_km'},
                        {'label': 'GDP per Capita', 'value': 'gdp_percap'},
                        {'label': 'Population', 'value': 'population_millions'},
                        {'label': 'Percent 65 and Older', 'value': 'age_percent_65_UP'},
                        {'label': 'Percent Smoking', 'value': 'percent_smoking_prev'},
                        {'label': 'COVID19 Related Deaths per Million', 'value': 'deaths_per_million'},
                        {'label': 'COVID19 Confirmed Cases per Million', 'value': 'cases_per_million'},
                        {'label': 'Spare Hospital Beds per Million', 'value': 'spare_beds_per_million'}],
            value = 'deaths_per_million',
            )]),
    html.Div([
        dcc.Slider(id = 'year_slider', min=min(covid_data['day_of_year']),
                    max=max(covid_data['day_of_year']), value=max(covid_data['day_of_year'])),
    ]),
])

@app.callback(
    Output(component_id='map', component_property='figure'),
    [Input(component_id='variable_selector', component_property='value'),
    Input(component_id='year_slider', component_property= 'value')],
)
def update_output(var_selected, year_selected):
    df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name', 'country_code']]

    map = go.Figure(data=go.Choropleth(
        locations = df['country_code'],
        locationmode = 'ISO-3',
        z = df[var_selected],
        colorscale = 'Reds',
        marker_line_color = 'black',
        marker_line_width = 0.5,
        ))
    map.update_layout(
        title_text = 'Confirmed Cases as of March 28, 2020',
        title_x = 0.5,
        geo=dict(
            showframe = False,
            showcoastlines = False,
            projection_type = 'equirectangular'
        ))

    return map

if __name__ == '__main__':
    app.run_server(debug=True)
