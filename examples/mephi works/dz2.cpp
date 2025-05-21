

#include <iostream>
#include <vector>
#include <cmath>
#include <string>


using namespace std;

const double e1 = 0.8;                              // степень черноты бочки
const double e2 = 0.5;                              // степень черноты бетоны
const double e = 1 / (1 / e1 + 1 / e2 - 1);         // приведена¤ степень черноты

const double g = 9.81;                              // гравитационна¤ посто¤нна¤ 
const double p0 = 100000;                           // атмосфрное давление
const double R = 8.31;                              // газова¤ посто¤нна¤
const double M = 0.02898;                           // мол¤рна¤ масса воздуха
const double sigma = 5.7 * pow(10, -8);             // ну это ¤ чисто
const double pi = 3.14;                             // число пи



double GetHydroDiam(double R2);     // возрващает гидравлический диаметр
double GetHeatCond(double T);       // возвращает коэф теплопроводности
double GetHeatCap(double T);        // возвращает теплоемкость
double GetDens(double T);           // возвращает плотность
double GetNu(double T);             // возвращает в¤зкость котора¤ ню
double GetMu(double T);             // возвращает в¤зкость котора¤ мю
double GetPr(double T);             // возвращает число прандтл¤
double GetNuselt(double W, double R2, double T);            // возвращает число нусельта дл¤ вынужденной конвекции
double GetAlpha(double W, double R2, double T);             // возвращает альфу из Ќютона рихмана 
double GetVelocity(double G, double R2, double T);          // возвращает скорость потока 
double GetReynolds(double W, double R2, double T);          // возвращает число рейнольдса
double GetKsi(double Re);
double GetTempCond(double T);
double GetTermExp(double T);
double GetCharSize(double R2);
double GetRa(double R2, double T, double Ts);
double GetG(double T, double R2, double T0);                // возвращает массовый поток
double GetT(double G, double Q, double T, double T0);       // возвращает среднюю температуру воздуха
double GetNuseltByRa(double R2, double T, double Ts, string Geometry);       // возвращает число нусельта дл¤ свободной конвекции в большом объеме
double GetAlphaByRa(double R2, double T, double Ts, string Geometry);        // возвращает альфу из ньютона рихмана дл¤ свободной конвекции
double GetQfromT_FreeFlow_BigSpace(double R2, double T, double Ts);           // возвращает энерговыделение дл¤ данной температуре в режиме свободной конвеции в большом объеме
double GetQfromT_FreeFlow_Channel(double R2, double T, double Ts);

double GetT1(double G, double Q, double R2, double T, double T1, double T2);        // возвращает значение “с1
double GetT2(double G, double Q, double R2, double T, double T1, double T2);        // возвращает значение “с2 

vector<double> FindT12(double G, double Q, double R2, double T);                    // решает систему ур-ний и возвращает вектор (“с1 , “с2)

vector<double> find_T_G(double Q, double T0, double R2);                            // решает систему ур-ний и возвращает вектор (T , G)



using namespace std;


const double h = 0.875;                             // высота бочки
const double R1 = 0.275;                            // радиус бочки



int main()
{
    //auto Q0 = GetQfromT_FreeFlow_BigSpace(1.8, -70, 90);
    //cout << Q0;
    /*
    for (double T = -70; T < 70; T += 10) {
        auto G = GetG(T, 0.350, -70);
        cout << endl << GetVelocity(G, 0.35, T) << "\t" << G;
    }
    cout << endl;
    */
    vector<double> R2 = { 0.350 };

    vector<double> T0 = { -70 };
    vector<double> Q = { 0, 100, 4000 , 5000 , 6000 , 7000 , 8000 , 9000, 10000, 11000, 12000 };

    double T1 = 0;
    double T2 = 0;
    auto q = Q[0];
    while (T1 < 90 && T2 < 70) {
        auto T_and_G = find_T_G(q, T0[0], R2[0]);
        auto T = T_and_G[0];
        auto G = T_and_G[1];

        auto T12 = FindT12(G, q, R2[0], T);
        T1 = T12[0];
        T2 = T12[1];
        cout << endl << q << "\t" << T << "\t" << G << "\t" << T1 << "\t" << T2;
        q += 100;

    }
    /*
    for (double t1 = -70; t1 < 80; t1 += 10) {
        cout << endl << GetHeatCap(t1);
    }
    */
    cout << endl << "-----------";
    double R_2 = 0.28;
    while (R_2 < 15) {
        double T1 = 0;
        double T2 = 0;
        auto q_1 = 90;
        while (T1 < 90 && T2 < 70) {
            q_1 += 10;
            auto T_and_G = find_T_G(q_1, T0[0], R_2);
            auto T = T_and_G[0];
            auto G = T_and_G[1];

            auto T12 = FindT12(G, q_1, R_2, T);
            T1 = T12[0];
            T2 = T12[1];
            /*
            cout << endl << "R2 = " << R - 2 << "\ttry q = " << q;
            cout << "\t" << "T1 = " << T1 << "\tT2 = " << T2;
            */


        }
        cout << endl << R_2 << "\t" << q_1 << "\t" << T1 << "\t" << T2;
        R_2 += +0.001;
    }

}


double GetHydroDiam(double R2)
{
    return 2 * (R2 - R1);
}
double GetDens(double T)
{
    return 353.089 / (T + 273.3);
}
double GetNu(double T)
{
    auto mu = GetMu(T);
    auto dens = GetDens(T);

    return mu / dens;
}
double GetMu(double T)
{
    return (17.1625 + 0.0482102 * T - 2.17419 * pow(10, -5) * T * T + 7.06065 * pow(10, -9) * T * T * T) / 1000000;
}
double GetPr(double T)
{
    auto mu = GetMu(T);
    auto Cp = GetHeatCap(T);
    auto l = GetHeatCond(T);
    return mu * Cp / l;
}
double GetNuselt(double W, double R2, double T)
{
    double Nuselt = 0;
    auto Re = GetReynolds(W, R2, T);
    auto Pr = GetPr(T);
    if (Re >= 200000)  Nuselt = 0.023 * pow(Re, 0.8) * pow(Pr, 0.4);
    else if (Re < 200000 && Re >= 1000) Nuselt = 0.26 * pow(Re, 0.6) * pow(Pr, 0.37);
    else if (Re < 1000 && Re >= 40) Nuselt = 0.52 * pow(Re, 0.5) * pow(Pr, 0.37);
    else if (Re < 40 && Re >= 10) Nuselt = 0.76 * pow(Re, 0.4) * pow(Pr, 0.37);

    auto Ra = GetRa(R2, T, T); // надо что-то придумать ... 

    return Nuselt;
}
double GetAlpha(double W, double R2, double T)
{
    auto Nuselt = GetNuselt(W, R2, T);
    auto d = GetCharSize(R2);
    auto l = GetHeatCond(T);
    return Nuselt * l / d;
}
double GetVelocity(double G, double R2, double T)
{
    auto ro = GetDens(T);
    double V = G / (ro * pi * (R2 * R2 - R1 * R1));
    return V;
}
double GetHeatCond(double T)
{
    return (2.41822 + 7.32841 * pow(10, -3) * T - 2.53698 * pow(10, -6) * T * T + 9.34274 * pow(10, -10) * T * T * T) / 100;
}
double GetHeatCap(double T)
{
    return 1.00564 * 1000 + 7.43322 * pow(10, -3) * T + 5.78429 * pow(10, -4) * T * T - 5.87508 * pow(10, -7) * T * T * T + 1.81359 * pow(10, -10) * pow(T, 4);
}
double GetReynolds(double W, double R2, double T)
{
    auto d = GetCharSize(R2);
    auto nu = GetNu(T);
    return W * d / nu;
}

inline double GetKsi(double Re)
{
    if (Re > 10000)  return 64 / Re;
    else return 0.316 / pow(Re, -0.25);
}

double GetTempCond(double T)
{
    double a = GetHeatCond(T) / (GetDens(T) * GetHeatCap(T));
    return a;
}

double GetTermExp(double T)
{
    double b = 1 / (T + 273.15);
    return b;
}

double GetCharSize(double R2)
{
    double d = 2 * (R2 - R1);
    double CharSize = (d * h) / (d + h);
    return CharSize;
}

double GetRa(double R2, double T, double Ts)
{
    double T_sr = (T + Ts) / 2;
    auto L = GetCharSize(R2);
    auto b = GetTermExp(T_sr);
    auto a = GetTempCond(T_sr);
    auto nu = GetNu(T_sr);

    double Ra = (g * pow(L, 3) * b * (Ts - T)) / (a * nu);

    return Ra;
}

double GetG(double T, double R2, double T0)
{
    auto ro = GetDens(T);
    auto nu = GetNu(T);
    auto d = GetCharSize(R2);
    double G = (2 * g * pow(d, 1.25)) / (0.316 * pow(nu, 0.25)); //перва¤ дробвь скобки
    G *= (T - T0) / (T0 + 273.15);                               //домножаем на вторую скобки 
    double power = 0.5714;
    G = pow(G, power);                                           //возводим скобку в степени
    //G *= (p0 * M) / (R * (T+273.15));                            //домножаем на дроб
    G *= ro * pi * (R2 * R2 - R1 * R1);

    return G;
}
double GetT(double G, double Q, double T, double T0)
{
    auto Cp = GetHeatCap(T);

    return T0 + Q / (2 * G * Cp);
}

double GetNuseltByRa(double R2, double T, double Ts, string Geometry)
{
    auto Ra = GetRa(1000, T, Ts);
    auto Pr = GetPr((T + Ts) / 2);
    double Gr = Ra / Pr;
    double Nuselt;
    // если теплообме в канале, то считаем дл¤ маленького объема
    if (Geometry == "Channel") {
        //double ek;
        //double C, n, K;

        //Nuselt = C * pow(Ra, n) * K;
    }
    // если большой объем, то считаем дл¤ большого объема
    else if (Geometry == "BigSpace") {
        auto Pr = GetPr(T);
        if (Ra > pow(10, 9)) {
            //cout << "\n" << "!!!!!" << "\n" << "warning!too big Ra" << "!!!!!";
            return 0.18 * pow(Ra, 0.333);
        }
        Nuselt = 0.68 + (0.67 * (Ra, 0.25)) / pow(1 + pow((0.492 / Pr), 0.5625), 0.4444);
    }

    return Nuselt;
}

double GetAlphaByRa(double R2, double T, double Ts, string Geomery)
{
    auto Nuselt = GetNuseltByRa(R2, T, Ts, "Channel");
    double T_sr = (T + Ts) / 2;
    auto l = GetHeatCond(T_sr);
    auto d = GetCharSize(R2);
    auto a = Nuselt * l / d;
    return a;
}

double GetQfromT_FreeFlow_BigSpace(double R2, double T, double Ts)
{
    auto a = GetAlphaByRa(R2, T, Ts, "BigSpace");
    double Q = a * (Ts - T) * 2 * pi * R1 * h + sigma * e1 * pow(Ts, 4);
    return 0.0;
}

double GetQfromT_FreeFlow_Channel(double R2, double T, double Ts)
{
    auto a = GetAlphaByRa(R2, T, Ts, "Channel");
    return 0.0;
}

double GetT1(double G, double Q, double R2, double T, double T1, double T2)
{
    if (G == 0) {
        return T;
    }
    auto W = GetVelocity(G, R2, T);
    auto alpha = GetAlpha(W, R2, (T + T1) / 2);

    double x = (1 / (alpha * R1)) * (Q / (2 * pi * h) + alpha * (T + 273.15) * R1 + sigma * e * (-pow(T1 + 273.15, 4) * R1 + pow(T2 + 273.15, 4) * R2 * (h / (h + R2 - R1))));
    return x - 273.15;
}

double GetT2(double G, double Q, double R2, double T, double T1, double T2)
{
    if (G == 0) {
        return T;
    }
    Q = 0;
    auto W = GetVelocity(G, R2, T);
    auto alpha = GetAlpha(W, R2, (T + T2) / 2);

    double x = (1 / (alpha * R2)) * (Q / (2 * pi * h) + alpha * (T + 273.15) * R2 + sigma * e * (-pow(T2 + 273.15, 4) * R2 + pow(T1 + 273.15, 4) * R1));
    return x - 273.15;
}

vector<double> FindT12(double G, double Q, double R2, double T)
{
    auto T1 = T;
    auto T2 = T;
    auto T1_old = T1;
    auto T2_old = T2;

    do {
        T1_old = T1;
        T2_old = T2;
        T1 = GetT1(G, Q, R2, T, T1, T2);
        T2 = GetT2(G, Q, R2, T, T1, T2);
    } while (abs(T1 - T1_old) > 0.1 || abs(T2 - T2_old) > 0.1);

    vector<double> ToReturn = { T1, T2 };

    return ToReturn;
}

vector<double> find_T_G(double Q, double T0, double R2)
{
    if (Q == 0) {
        vector<double> answer = { T0, 0 };
        return answer;
    }

    double T = T0 + 10;
    double G = GetG(T, R2, T0);
    double Gold = 0;
    double Told = 0;
    do {
        Gold = G;
        Told = T;
        G = GetG(T, R2, T0);
        T = GetT(G, Q, T, T0);
    } while (abs(T - Told) > 0.1 || abs((G - Gold) / G) > 0.01);

    vector<double> answer = { T, G };
    return answer;
}