import pandas as pd
import numpy as np
import datetime as dt
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.express as px
import plotly.graph_objs as go


app = dash.Dash()

covid_data = pd.read_csv("data/covid_country_level_data.csv")
covid_data.drop(columns = ['s1_is_general', 's2_is_general', 's3_is_general', 's4_is_general',
                           's5_is_general', 's6_is_general', 'stringency_index', 'age_percent_0_to_14',
                           'age_percent_15_to_64'], inplace = True)

app.layout = html.Div([


    html.H1(['Interact with the charts below to see how COVID-19 and State Responses are affecting the world'], style={'text-align':'center', 'font-size':'20px', 'font-family':'sans-serif'}),


    html.Div([
        dcc.Markdown(id = 'date_display', style={'text-align':'center', 'font-weight':'bold', 'font-size':'30px', 'font-family':'sans-serif'}),
        html.P("Use Slider to Adjust the Date", style={'text-align': 'right', 'margin-right': '18px', 'font-size': '14px', 'font-family': 'sans-serif'}),
        dcc.Slider(id = 'date_slider', min=min(covid_data['day_of_year'])+9,
                    max=max(covid_data['day_of_year']), value=max(covid_data['day_of_year'])-1,
                    marks = {
                        min(covid_data['day_of_year'])+9: {'label': str((dt.datetime(2020, 1, 1) + dt.timedelta(min(covid_data['day_of_year'])+8)).strftime("%b %d, %Y"))},
                        #max(covid_data['day_of_year'])-5: {'label': str((dt.datetime(2020, 1, 1) + dt.timedelta(max(covid_data['day_of_year'])-6)).strftime("%b %d, %y"))}
                    }),
    ]),


    html.Div([

        html.Div([
            dcc.Dropdown(id="variable_selector",
                options =  [{'label': 'Confirmed Cases', 'value': 'confirmed_cases'},
                            {'label': 'Confirmed Deaths', 'value': 'confirmed_deaths'},
                            {'label': 'School Closing', 'value': 's1_school_closing'},
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
                value = 'confirmed_deaths',
                )]),

                html.Div([dcc.Graph(id='map')])],

        style={'width':'60%',  'display':'inline-block'}
    ),


    html.Div([

        html.Div([
            dcc.Dropdown(id="line_selector",
                options = [{'label': 'Confirmed Cases', 'value': 'confirmed_cases'},
                           {'label': 'Confirmed Deaths', 'value': 'confirmed_deaths'},
                           {'label': 'COVID19 Related Deaths per Million', 'value': 'deaths_per_million'},
                           {'label': 'COVID19 Confirmed Cases per Million', 'value': 'cases_per_million'},
                           {'label': 'Spare Hospital Beds per Million', 'value': 'spare_beds_per_million'}],
                           value = 'confirmed_deaths')
                           ]),

        html.Div([dcc.Graph(id = 'growth')])],

        style={'width':'40%', 'display':'inline-block'}
    ),

    html.Div([
    dcc.Markdown(id="covid_stats")
    ], style={'text-align':'center', 'font-family':'sans-serif'})
])


@app.callback(
    Output(component_id='map', component_property='figure'),
    [Input(component_id='variable_selector', component_property='value'),
    Input(component_id='date_slider', component_property= 'value')],
)
def update_map(var_selected, date_selected):
    df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name']].query(f"day_of_year=={date_selected}")

    map = go.Figure(data=go.Choropleth(
        locations = df['country_name'],
        locationmode = 'country names',
        z = np.log(df[var_selected]),
        colorscale = 'Reds',
        marker_line_color = 'black',
        marker_line_width = 0.5,
        zmin=0,
        zmax=max(np.log(covid_data[var_selected])),
        text = df[var_selected],
        hoverinfo = 'location+text'
        ))
    map.update_layout(
        title_x = 0.5,
        margin=dict(
        l=5,
        r=50,
        b=0,
        t=0,
        pad=2
        ),
        geo=dict(
            showframe = False,
            showcoastlines = True,
            projection_type = 'equirectangular',
        ))

    return map


@app.callback(
    Output(component_id='growth', component_property='figure'),
    [Input(component_id='line_selector', component_property='value'),
     Input(component_id='date_slider', component_property= 'value')]
)
def update_graph(var_selected, date_selected):
    if('death' in var_selected):
        df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                        'days_since_first_death']].query(f"days_since_first_death>0 & day_of_year <= {date_selected}")
        fig = px.line(df, x='days_since_first_death', y=var_selected, color='country_name',
                        line_shape='spline', render_mode='svg', hover_name='country_name',)
        fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                            margin=dict(
                            l=0,
                            r=50,
                            b=50,
                            t=0,
                            pad=0
                            ))
    else:
        df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                        'days_since_first_case']].query(f"days_since_first_case>0 & day_of_year <= {date_selected}")
        fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name')
        fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                            margin=dict(
                            l=0,
                            r=50,
                            b=50,
                            t=0,
                            pad=0
                            ))

    return fig


@app.callback(
    Output(component_id='date_display', component_property='children'),
    [Input(component_id='date_slider', component_property= 'value')]
)
def display_date(date_selected):
    return (dt.datetime(2020, 1, 1) + dt.timedelta(date_selected - 1)).strftime("%B %d, %Y")


@app.callback(
    Output(component_id='covid_stats', component_property='children'),
    [Input(component_id='line_selector', component_property='value'),
     Input(component_id='date_slider', component_property= 'value')]
)
def covid_stats(var_selected, date_selected):
    df = covid_data[['day_of_year', 'date', 'country_name', 'confirmed_cases', 'confirmed_deaths',
                    'stringency_index_for_display', 's1_school_closing', 's6_restrictions_on_internal_movement']].query(f"day_of_year=={date_selected}")

    summary = f"""
    Global Statistics \n
    Confirmed Cases: {df['confirmed_cases'].sum()} |
    Countries who've closed schools: {np.count_nonzero(df['s1_school_closing'])} |
    Countries who've restricted internal movement: {np.count_nonzero(df['s6_restrictions_on_internal_movement'])}
    """
    return summary



if __name__ == '__main__':
    app.run_server(debug=True)
