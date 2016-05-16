int main () {
	int i;
	int wow;
	wow = 0;

	for (i = 0; i < 10; i = i + 1) {
		int j;
		j = 0;
		while (j < 10) {
			wow = wow + j * i;
			j = j + 1;
		}
	}

	return 0;
}
