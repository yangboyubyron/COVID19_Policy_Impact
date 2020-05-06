import pandas as pd
import numpy as np
import datetime as dt
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.express as px
import plotly.graph_objs as go
from dash.exceptions import PreventUpdate
import locale
locale.setlocale(locale.LC_ALL, 'en_US')


app = dash.Dash()

covid_data = pd.read_csv("data/covid_country_level_data.csv")
covid_data.drop(columns = ['m1_wildcard', 'stringency_index','legacy_stringency_index_for_display', 'legacy_stringency_index', 'age_percent_0_to_14',
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
                    }),
    ]),

    html.Div([

    html.Div([
        html.Div([
            dcc.Dropdown(id="variable_selector",
                options =  [{'label': 'Confirmed Cases', 'value': 'confirmed_cases'},
                            {'label': 'Confirmed Deaths', 'value': 'confirmed_deaths'},
                            {'label': 'School Closing', 'value': 'c1_school_closing'},
                            {'label': 'Restrict Gatherings', 'value': 'c4_restrictions_on_gatherings'},
                            {'label': 'Workplace Closing', 'value': 'c2_workplace_closing'},
                            {'label': 'Stay at Home Requirements', 'value': 'c6_stay_at_home_requirements' },
                            {'label': 'Public Events Cancelled', 'value': 'c3_cancel_public_events'},
                            {'label': 'Public Transportation Closed', 'value': 'c5_close_public_transport'},
                            {'label': 'Public Information Campaigns', 'value': 'h1_public_information_campaigns'},
                            {'label': 'Restrictions on Internal Movement', 'value': 'c7_restrictions_on_internal_movement'},
                            {'label': 'International Travel Controls', 'value': 'c8_international_travel_controls'},
                            {'label': 'Fiscal Measures', 'value': 'e3_fiscal_measures'},
                            {'label': 'Testing Policy', 'value':'h2_testing_policy'},
                            {'label': 'Contact Tracing', 'value': 'h3_contact_tracing'},
                            {'label': 'Emergency Investment in Health Care', 'value': 'h4_emergency_investment_in_health_care'},
                            {'label': 'Investment in Vaccines', 'value': 'h5_investment_in_vaccines'},
                            {'label': 'Income Support', 'value': 'e1_income_support'},
                            {'label': 'Debt Contract Relief', 'value': 'e2_debt_contract_relief'},
                            {'label': 'Recieving International Support', 'value': 'e4_international_support'},
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
                value = 'confirmed_deaths', placeholder = "Select a Metric to Observe"
                )], style = {'width': '60%', 'display':'inline-block'}),

                html.Div([dcc.Dropdown(id="line_selector",
                    options = [{'label': 'Confirmed Cases', 'value': 'confirmed_cases'},
                               {'label': 'Confirmed Deaths', 'value': 'confirmed_deaths'},
                               {'label': 'COVID19 Related Deaths per Million', 'value': 'deaths_per_million'},
                               {'label': 'COVID19 Confirmed Cases per Million', 'value': 'cases_per_million'},
                               {'label': 'Estimated Spare Hospital Beds per Million', 'value': 'spare_beds_per_million'}],
                               value = 'confirmed_deaths', placeholder = "Select a Metric to Observe")], style = {'width':'20%', 'display':'inline-block'}),

                html.Div([dcc.Dropdown(id="country_selector",
                    options = [{'label': f"{value}", 'value':f"{value}"} for value in covid_data['country_name'].unique()],
                               multi=True, placeholder = "Select Specific Countries")
                               ], style = {'width':'20%' , 'display':'inline-block'})

    ]),

    html.Div([
        dcc.Graph(id='map')
        ],style={'width':'60%',  'display':'inline-block'}),

    html.Div([
        dcc.Graph(id = 'growth')
        ],style={'width':'40%', 'display':'inline-block'})

    ]),

    html.Div([
    dcc.Markdown(id="covid_stats")
    ], style={'text-align':'center', 'font-family':'sans-serif'}),

    html.Div([
        html.P(["Created by: Brian VandenAkker"], style= {'text-align': 'left','font-size': '10px', 'display':'inline-block', 'width':'20%'}),
        html.P(["Primary Data Source: Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira.(2020). Oxford COVID-19 Government Response Tracker. Blavatnik School of Government."], style = {'text-align': 'right','font-size': '10px', 'display':'inline-block', 'width':'70%'})
],style = {'text-align': 'center'}
)])


@app.callback(
    Output(component_id='map', component_property='figure'),
    [Input(component_id='variable_selector', component_property='value'),
    Input(component_id='date_slider', component_property= 'value')],
)
def update_map(var_selected, date_selected):
    if var_selected is None:
        raise PreventUpdate

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
     Input(component_id='country_selector', component_property='value'),
     Input(component_id='date_slider', component_property= 'value')]
)
def update_graph(var_selected, country_selected, date_selected):
    if var_selected is None:
        raise PreventUpdate
    elif country_selected is None:
        if('death' in var_selected):
            df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                            'days_since_first_death']].query(f"days_since_first_death>0 & day_of_year <= {date_selected}")
            fig = px.line(df, x='days_since_first_death', y=var_selected, color='country_name',
                            line_shape='spline', render_mode='svg', hover_name='country_name',
                            )
            fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                                margin=dict(
                                l=0,
                                r=50,
                                b=50,
                                t=0,
                                pad=0
                                )),
            fig.update_xaxes(title_text= 'Days Since First Death'),
            fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())

        elif(var_selected == 'spare_beds_per_million'):
            df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                            'days_since_first_case']].query(f"days_since_first_case>0 & day_of_year <= {date_selected}")
            fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name',
                            line_shape='spline', render_mode='svg', hover_name='country_name',
                            )
            fig.update_layout(showlegend=False, plot_bgcolor='white',
                                margin=dict(
                                l=0,
                                r=50,
                                b=50,
                                t=0,
                                pad=0
                                )),
            fig.update_xaxes(title_text= 'Days Since First Death'),
            if(min(df[var_selected]) < 0 ): val = 1.2
            else: val = -1.2
            fig.update_yaxes(range = [min(df[var_selected])*val, max(df[var_selected])*1.2], title_text = var_selected.replace('_', ' ').title())

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
                                )),
            fig.update_xaxes(title_text= 'Days Since First Case'),
            fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())

    else:
        if('death' in var_selected):

            df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                            'days_since_first_death']].query(f"days_since_first_death>0 & day_of_year <= {date_selected} & country_name == {country_selected}")
            try:
                fig = px.line(df, x='days_since_first_death', y=var_selected, color='country_name',
                                line_shape='spline', render_mode='svg', hover_name='country_name')
                fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                                    margin=dict(
                                    l=0,
                                    r=50,
                                    b=50,
                                    t=0,
                                    pad=0
                                    )),
                fig.update_xaxes(title_text= 'Days Since First Death'),
                fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())
            except:
                df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                                'days_since_first_death']].query(f"days_since_first_death>0 & day_of_year <= {date_selected}")
                fig = px.line(df, x='days_since_first_death', y=var_selected, color='country_name',
                                line_shape='spline', render_mode='svg', hover_name='country_name')
                fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                                    margin=dict(
                                    l=0,
                                    r=50,
                                    b=50,
                                    t=0,
                                    pad=0
                                    )),
                fig.update_xaxes(title_text= 'Days Since First Death'),
                fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())
        else:
            df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                            'days_since_first_case']].query(f"days_since_first_case>0 & day_of_year <= {date_selected} & country_name == {country_selected}")
            try:
                if(var_selected == 'spare_beds_per_million'):
                    fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name',
                                    line_shape='spline', render_mode='svg', hover_name='country_name',
                                    )
                    fig.update_layout(showlegend=False, plot_bgcolor='white',
                                        margin=dict(
                                        l=0,
                                        r=50,
                                        b=50,
                                        t=0,
                                        pad=0
                                        )),
                    fig.update_xaxes(title_text= 'Days Since First Death'),
                    if(min(df[var_selected]) < 0 ): val = 1.2
                    else: val = -1.2
                    fig.update_yaxes(range = [min(df[var_selected])*val, max(df[var_selected])*1.2], title_text = var_selected.replace('_', ' ').title())
                else:
                    fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name')
                    fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                                        margin=dict(
                                        l=0,
                                        r=50,
                                        b=50,
                                        t=0,
                                        pad=0
                                        )),
                    fig.update_xaxes(title_text= 'Days Since First Case'),
                    fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())

            except:
                if(var_selected == 'spare_beds_per_million'):
                    df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                                    'days_since_first_case']].query(f"days_since_first_case>0 & day_of_year <= {date_selected}")
                    fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name',
                                    line_shape='spline', render_mode='svg', hover_name='country_name',
                                    )
                    fig.update_layout(showlegend=False, plot_bgcolor='white',
                                        margin=dict(
                                        l=0,
                                        r=50,
                                        b=50,
                                        t=0,
                                        pad=0
                                        )),
                    fig.update_xaxes(title_text= 'Days Since First Death'),
                    if(min(df[var_selected]) < 0 ): val = 1.2
                    else: val = -1.2
                    fig.update_yaxes(range = [min(df[var_selected])*val, max(df[var_selected])*1.2], title_text = var_selected.replace('_', ' ').title())
                else:
                    df = covid_data[[var_selected, 'day_of_year', 'date', 'country_name',
                                    'days_since_first_case']].query(f"days_since_first_case>0 & day_of_year <= {date_selected}")
                    fig = px.line(df, x='days_since_first_case', y=var_selected, color='country_name',
                                    line_shape='spline', render_mode='svg', hover_name='country_name')
                    fig.update_layout(yaxis_type="log", showlegend=False, plot_bgcolor='white',
                                        margin=dict(
                                        l=0,
                                        r=50,
                                        b=50,
                                        t=0,
                                        pad=0
                                        )),
                    fig.update_xaxes(title_text= 'Days Since First Case'),
                    fig.update_yaxes(title_text = var_selected.replace('_', ' ').title())


    return fig


@app.callback(
    Output(component_id='date_display', component_property='children'),
    [Input(component_id='date_slider', component_property= 'value')]
)
def display_date(date_selected):
    return (dt.datetime(2020, 1, 1) + dt.timedelta(date_selected - 1)).strftime("%B %d, %Y")


@app.callback(
    Output(component_id='covid_stats', component_property='children'),
    [Input(component_id='date_slider', component_property= 'value')]
)
def covid_stats(date_selected):
    df = covid_data[['day_of_year', 'date', 'country_name', 'confirmed_cases', 'confirmed_deaths',
                    'stringency_index_for_display', 'c1_school_closing', 'c2_workplace_closing', 'c6_stay_at_home_requirements']].query(f"day_of_year=={date_selected}")

    summary = f"""
    Global Statistics \n
    Confirmed Cases: {locale.format("%d", df['confirmed_cases'].sum(), grouping=True)} |
    Total Deaths: {locale.format("%d", np.sum(df['confirmed_deaths']), grouping = True)} |
    Countries with workplace closings: {(np.count_nonzero(df['c2_workplace_closing']))} |
    Countries with state at home requirements: {np.count_nonzero(df['c6_stay_at_home_requirements'])}
    """
    return summary



if __name__ == '__main__':
    app.run_server(debug=True)
