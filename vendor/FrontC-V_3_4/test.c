
int quantum_ipow(int a, int b)
{
	int i;
	int r = 1;
	for(i = 0; i < b; i++)
		r *= a;
	return r;
}

int quantum_gcd(int u, int v)
{
	int r;
	while(v)
	{
		r = u % v;
		u = v;
		v = r;
	}
	return u;
}

void quantum_frac_approx(int *a, int *b, int width)
{
	float f = (float)(*a) / *b;
	float g = f;
	int i, num2 = 0, den2 = 1, num1 = 1, den1 = 0, num = 0, den = 0;
	do
	{
		i = (int)(g + 0.000005);
		g -= i - 0.000005;
		g = 1.0 / g;
		if(i * den1 + den2 > 1 << width)
			break;
		num = i * num1 + num2;
		den = i * den1 + den2;
		num2 = num1;
		den2 = den1;
		num1 = num;
		den1 = den;
	}
	while(fabs((double)num / den - f) > 1.0 / (2 * (1 << width)));
	*a = num;
	*b = den;
	return;
}

int quantum_getwidth(int n)
{
	int i;
	for(i = 1; 1 << i < n; i++)
		;
	return i;
}

int quantum_inverse_mod(int n, int c)
{
	int i;
	for(i = 1; i * c % n != 1; i++)
		;
	return i;
}

