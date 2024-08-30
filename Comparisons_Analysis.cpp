#include <bits/stdc++.h>
#include <random>
using namespace std;
#define ll long long


vector<float> Randomized_median(vector<float>& Inp, float power) {
    // From here Implementation of the algorithm starts
    srand(time(NULL));
    int n = Inp.size(); // Length of the input
    int expec = ceil(pow(n, power)); // Expected size of random sample
    float comp = 0;
    int t = ceil(sqrt(expec * log(n))); // parameter for finding a and b
    float p = static_cast<float>(expec) / n; // probability of selection of each element in random sample from Input array
    float INFINITy = numeric_limits<float>::infinity(); 

    // Random Sampling
    vector<float> random_sample;
    random_device rd;
    mt19937 e2(rd());
    uniform_real_distribution<float> distr(0,1);
    for (auto x:Inp) 
    {
        float U = distr(e2);
        if (U <= p) {
            random_sample.push_back(x);
        }
    }
    int l = random_sample.size();

    // Check for size of Random Sample
    ++comp;
    if (l > 2 * expec) {
        cout << "Algorithm gone through bad random sample of the array. Please run it again!" << std::endl;
        return {INFINITy, comp};
    }

    // Sorting Random Sample (Takes only o(n) time)
    unsigned int ncomp = 0;
    partial_sort(random_sample.begin(), random_sample.begin()+min(((expec)/2 + t + 3),l), random_sample.end(), [&ncomp]( float lhs, float rhs ) 
    {
        ++ncomp;
        return lhs < rhs;
    });
    comp += ncomp;

    // Fixing a and b
    float a = random_sample[ceil(static_cast<float>(expec)/2 - t + 1)];
    float b = random_sample[ceil(static_cast<float>(expec)/2 + t + 1)];
    random_sample.clear();

    // Finding rank of a in original array (Takes n comparison)
    vector<float> Inp_1;
    for (auto x:Inp) {
        if (a < x) {
            Inp_1.push_back(x);
        }
    }
    int rank_a = n - Inp_1.size();
    comp += (n + 2);
    if (rank_a > n / 2) {
        cout << "Algorithm gone through bad random sample of the array. Please run it again!" << std::endl;
        return {INFINITy, comp};
    }

    // Finding rank of b in original array (Takes 0.5n + o(n) comparison)
    vector<float> Inp_2;
    for (float val : Inp_1)
    {
        if (b > val) {
            Inp_2.push_back(val);
        }
    }
    int rank_b = rank_a + Inp_2.size();
    comp += (Inp_1.size() + 2);
    Inp_1.clear();
    if (rank_b < n / 2) {
        cout << "Algorithm gone through bad random sample of the array. Please run it again!" << endl;
        return {INFINITy, comp};
    }

    // Returning median
    float med;
    unsigned int ncomp1 = 0;
    partial_sort(Inp_2.begin(),Inp_2.begin()+n/2 - rank_a+1, Inp_2.end(),[&ncomp1]( float lhs, float rhs ) 
    {
        ++ncomp1;
        return lhs < rhs;
    });
    med = (Inp_2[n / 2 - rank_a]);
    comp += ncomp1;
    return {med, comp};
}


int main()
{
int sizes[] = {100,200,400,600,800,900,1000,2000,4000,6000,8000,10000,20000,40000,60000,80000,100000,200000,400000,600000,800000,1000000,2000000,4000000,6000000,8000000,10000000,20000000,40000000,60000000,80000000, 100000000};
vector<ll> comp1,comp2;
    for(int n:sizes)
    {
        vector<float> randArray(n);
        unsigned int c1=0,c2=0;
        for(int k=0;k<20;k++)
        {
            for(auto &x:randArray)
            {
                x = -10000000 + (20000000)*((float)rand() / RAND_MAX);
            }
            vector<float> med1 = Randomized_median(randArray, 0.67);
            unsigned int dcomp = 0;
            nth_element(randArray.begin(),randArray.begin()+n/2, randArray.end(),[&dcomp]( float lhs, float rhs ) 
            {
                ++dcomp;
                return lhs < rhs;
            });
            c1 = c1 + (ll)med1[1];
            c2 = c2 + (ll)dcomp;
        }
        comp1.push_back(c1/20);
        comp2.push_back(c2/20);
    }

    for(int i=0;i<comp1.size();i++)
    {
        cout<<comp1[i]<<", ";
    }
    cout<<"\n";
    for(int i=0;i<comp2.size();i++)
    {
        cout<<comp2[i]<<", ";
    }
return 0;
}
