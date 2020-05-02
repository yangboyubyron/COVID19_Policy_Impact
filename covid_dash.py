import pandas as pd
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import plotly.express as px

from dash.exceptions import PreventUpdate

app = dash.Dash()

covid_data = pd.read_csv("data/covid_country_level_data.csv")

app.layout = html.Div([
    html.Div([
        dcc.Graph(id='map')
    ]),
    html.Div([
        dcc.Graph(id = 'growth')
    ]),
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
        dcc.Slider(id = 'year_slider', min=min(covid_data['day_of_year']), max=max(covid_data['day_of_year']), step=1, value=1),
    ]),
])

@app.callback(
    [Output(component_id='map', component_property='figure'),
    Output(component_id='growth', component_property='figure')],
    [Input(component_id='variable_selector', component_property='value'),
    Input(component_id='year_slider', component_property= 'value')],
)
def update_output(var_selected, year_selected):
    if var_selected is None:
        raise PreventUpdate
    else:
        df = covid_data.query(f"day_of_year=={year_selected}")
        #['date', 'day_of_year', 'country_code', 'country_name', str(var_selected)]
        print(df)

        map = px.choropleth(df, locations="country_code", 
                            color="deaths_per_million",
                            hover_name="country_name",
                            projection='natural earth',
                            title=f'{var_selected}',
                            color_continuous_scale=px.colors.sequential.Plasma)

        map.update_layout(title=dict(font=dict(size=28),x=0.5,xanchor='center'),
                          margin=dict(l=60, r=60, t=50, b=50))

        g_fig = px.line(df, x = "date", y= var_selected, color="country_code", title=f'{var_selected}')

        return (map, g_fig)

if __name__ == '__main__':
    app.run_server(debug=True)
