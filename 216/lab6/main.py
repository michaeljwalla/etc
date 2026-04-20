'''
michael walla 4/20
for i in {1..3}; do python3 main.py < tests/$i; done
'''

from enum import Enum
import csv
from statistics import mean, stdev
from math import pi

class CSVField(Enum):
    Timestamp = "timestamp"
    Green = "position_px_y-green"


def get_y_color(c: CSVField, f:str) -> list[tuple[float, float]]:
    out: list[tuple[float,float]] = []
    with open(f) as file:
        cdata = csv.reader(file)
        header = next(cdata)
        ts, col = header.index(CSVField.Timestamp.value), header.index(c.value)
        for row in cdata:
            out.append( (float(row[ts]), float(row[col])) )

    return out

#min max mean
def bounds(d: list[tuple[float,float]])->list[float]:
    return [
        min(d,key=lambda i:i[1])[1],
        max(d,key=lambda i:i[1])[1],
        mean([i[1] for i in d])
    ]

within = lambda x,y,e: abs(x-y) < e # type: ignore
def get_near(d: list[tuple[float,float]], goal:float, eps:float=15) -> list[tuple[float,float]]:
    out:list[tuple[float,float]] = []
    i:int = 0

    while i < len(d):
        _,y = d[i]
        run:list[tuple[float,float]] = []
        
        #add those close
        while within(goal, y, eps) and i < len(d):
            _,y = d[i]
            run.append( d[i] )
            i += 1
        
        #append the closest
        if len(run):
            out.append( min(run, key=lambda i: abs(i[1]-goal) ) )
        else:
            i += 1
    #
    return out

#
def main():
    f:str = input("filename: ")
    Data = get_y_color(CSVField.Green, f)

    print(f)
    print("Counting Peak Time Diffs\n")
    min,max,_mid = bounds(Data)
    #
    peaks = get_near(Data, max)
    troughs = get_near(Data, min)
    oscillations = [ peaks[i+1][0] - peaks[i][0] for i in range(len(peaks)-1) ]
    print(len(peaks),"peaks", len(troughs),"troughs", len(oscillations), "oscillations")


    amplitude = (max-min)/2 * 1e-3              # m
    period = sum(oscillations) / len(oscillations) * 1e-3 # s

    print(f"{amplitude=:.4f}m, {period=:.4f}s\n")
    mass = float(input("mass weight (g): ")) * 1e-3

    omega = 2*pi/period #ang freq
    k = omega**2 * mass

    print(f"{omega=:.4f} rad/s, {k=:.4f} N/m")

    print("Uncertainty")
    sigma_pd: float = stdev(i*1e-3 for i in oscillations) / len(oscillations)**.5
    
    '''
    ang = 2pi/T
    d/dt -> -2pi/T^2 * unc(T)
    '''
    sigma_om:float = -2*pi/period**2 * sigma_pd

    '''
    k = w^2*m
    d/dm -> w^2 * unc(m)
    d/dw -> 2wm * unc(w)

    unc(k) = sqrt( a^2 + b^2  )
    '''
    sigma_m = 0.0 #assuming based off of given num
    sigma_k: float = (\
        ( omega**2 * sigma_m )**2 + \
        ( 2*omega*mass * sigma_om )**2\
    )**0.5

    print(f"{sigma_pd=:.4f}")
    print(f"{sigma_om=:.4f}")
    print(f"{sigma_m=:.4f}")
    print(f"{sigma_k=:.4f}")
    print("-"*50)


    

#
if __name__ == "__main__":
    main()