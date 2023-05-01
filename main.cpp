//Nekrasov Dmitrii DSAI-02
//d.nekrasov@innopolis.university
#include <iostream>
#include <vector>
#include <iomanip>
#include <valarray>
#include<cmath>

using namespace std;

class ColumnVector {
public:
    int size;
    vector<double> data;

    ColumnVector() = default;

    ColumnVector(vector<double> data) {
        this->size = data.size();
        this->data = data;
    }

    ColumnVector(int size) {
        this->size = size;
        this->data.resize(this->size);
    }

    ColumnVector operator+(ColumnVector &columnVector2) {
        ColumnVector res(this->size);
        for (int i = 0; i < this->data.size(); i++)
            res.data[i] = this->data[i] + columnVector2.data[i];
        return res;
    }

    ColumnVector operator-(ColumnVector &columnVector2) {
        ColumnVector res(this->size);
        for (int i = 0; i < this->data.size(); i++)
            res.data[i] = this->data[i] - columnVector2.data[i];
        return res;
    }

    ColumnVector operator*(ColumnVector &columnVector2) {
        ColumnVector res(this->size);
        for (int i = 0; i < this->data.size(); i++)
            res.data[i] = this->data[i] * columnVector2.data[i];
        return res;
    }

//    ColumnVector operator=(ColumnVector& columnVector2){
//        for (int i=0;i<this->data.size();i++)
//            this->data[i]=columnVector2.data[i];
//        return {data};
//    }
    ColumnVector operator=(ColumnVector columnVector2) {
        this->size = columnVector2.size;
        for (int i = 0; i < this->data.size(); i++)
            this->data[i] = columnVector2.data[i];
        return {data};
    }

    friend istream &operator>>(istream &is, ColumnVector &columnvector1) {
        is >> columnvector1.size;
        columnvector1.data.resize(columnvector1.size);
        for (int i = 0; i < columnvector1.size; i++)
            is >> columnvector1.data[i];
        return is;
    }

    friend ostream &operator<<(ostream &os, ColumnVector &columnVector1) {
        for (int i = 0; i < columnVector1.size; i++) {
            if (columnVector1.data[i] >= 0)
                columnVector1.data[i] = max(columnVector1.data[i], 0.00);
            if (columnVector1.data[i] <= 0 and fabs(columnVector1.data[i]) <= 0.0001)
                columnVector1.data[i] = 0.00;
            os << columnVector1.data[i] << "\n";
        }
        return os;
    }

    double getNorm() {
        double norm = 0;
        for (double i: data)
            norm += (i * i);
        return sqrt(norm);
    }
};

class Matrix;

void swaprows(Matrix &a, int i, int j);

void subtractRow(Matrix &a, int j, int i);

class AugmentedMatrix;

class SquareMatrix;

class Matrix {
public:
    int row;
    int column;
    vector<vector<double>> table;

    Matrix() {
        this->row = 0;
        this->column = 0;
        this->table.resize(row, vector<double>(column, 0.0));
    }

    Matrix(int row, int column) {
        this->row = row;
        this->column = column;
        table.resize(row, vector<double>(column, 0.0));
    }

    Matrix transpose() {
        vector<vector<double>> newTable;
        vector<double> tableRow;
        Matrix res;
        res.row = this->column;
        res.column = this->row;
        for (int i = 0; i < column; i++) {
            for (int j = 0; j < row; j++)
                tableRow.push_back(this->table[j][i]);
            newTable.push_back(tableRow);
            tableRow.clear();
        }
        res.table = newTable;
        return res;
    }

    Matrix operator+(Matrix a) const {
        if (this->column == a.column && this->row == a.row) {
            Matrix res;
            res.row = this->row;
            res.column = this->column;
            res.table = this->table;
            vector<vector<double>> temptable = this->table;
            for (int i = 0; i < this->column; i++)
                for (int j = 0; j < this->row; j++)
                    temptable[j][i] += a.table[j][i];
            res.table = temptable;
            return res;
        } else {
            cout << "Error: the dimensional problem occurred\n";
            Matrix res;
            return res;
        }
    }

    Matrix operator-(Matrix a) const {
        if (this->column == a.column && this->row == a.row) {
            Matrix res;
            res.row = this->row;
            res.column = this->column;
            res.table = this->table;
            vector<vector<double>> temptable = this->table;
            for (int i = 0; i < this->column; i++)
                for (int j = 0; j < this->row; j++)
                    temptable[j][i] -= a.table[j][i];
            res.table = temptable;
            return res;
        } else {
            cout << "Error: the dimensional problem occurred\n";
            Matrix res;
            return res;
        }
    }

    Matrix operator*(Matrix a) {
        if (this->column == a.row) {
            Matrix res;
            res.row = this->row;
            res.column = a.column;
            vector<double> newRow;
            double sum = 0;
            for (int i = 0; i < this->row; i++) {
                for (int j = 0; j < a.column; j++) {
                    for (int k = 0; k < this->column; k++)
                        sum += this->table[i][k] * a.table[k][j];
                    newRow.push_back(sum);
                    sum = 0;
                }
                res.table.push_back(newRow);
                newRow.clear();
            }
            return res;
        } else {
            cout << "Error: the dimensional problem occurred\n";
            Matrix res;
            return res;
        }
    }

    ColumnVector operator*(ColumnVector columnvector1) {
        if (this->column == columnvector1.size) {
            ColumnVector res;
            res.size = this->row;
            res.data.resize(res.size);
            double sum;
            for (int i = 0; i < this->row; i++) {
                sum = 0;
                for (int j = 0; j < this->column; j++)
                    sum += this->table[i][j] * columnvector1.data[j];
                res.data[i] = sum;
            }
            return res;
        } else {
            cout << "Error: the dimensional problem occurred\n";
            ColumnVector res;
            return res;
        }
    }

    friend void swap(Matrix &matrix1, Matrix matrix2) {
        swap(matrix1.column, matrix2.column);
        swap(matrix1.row, matrix2.row);
        swap(matrix1.table, matrix2.table);
    }

    Matrix &operator=(const Matrix &other) {
        if (this != &other) {
            this->row = other.row;
            this->column = other.column;
            this->table = other.table;
        }
        return *this;
    }

    //operator >>overload
    friend istream &operator>>(istream &is, Matrix &matrix1) {
        is >> matrix1.row >> matrix1.column;
        matrix1.table.resize(matrix1.row, vector<double>(matrix1.column, 0.0));
        for (int i = 0; i < matrix1.row; i++)
            for (int j = 0; j < matrix1.column; j++)
                is >> matrix1.table[i][j];
        return is;
    }

    //operator << overload
    friend ostream &operator<<(ostream &os, Matrix &matrix1) {
        for (int i = 0; i < matrix1.row; ++i) {
            for (int j = 0; j < matrix1.column; ++j) {
                if (matrix1.table[i][j] >= 0)
                    matrix1.table[i][j] = max(matrix1.table[i][j], 0.00);
                if (matrix1.table[i][j] <= 0 and fabs(matrix1.table[i][j]) <= 0.0001)
                    matrix1.table[i][j] = 0.00;
                os << matrix1.table[i][j] << ' ';
            }
            os << '\n';
        }
        return os;
    }
};

void swaprows(Matrix &a, int i, int j) {
    double tempdouble = -999;
    for (int k = 0; k < a.column; k++) {
        tempdouble = a.table[i][k];
        a.table[i][k] = a.table[j][k];
        a.table[j][k] = tempdouble;
    }
}

//row j-=(coef*row i)
void subtractRow(Matrix &a, int j, int i) {
    double coef = a.table[j][i] / a.table[i][i];
    for (int k = 0; k < a.column; k++)
        a.table[j][k] -= (coef * a.table[i][k]);
}

class SquareMatrix : public Matrix {
public:
    SquareMatrix() = default;

    SquareMatrix(Matrix &convert) {
        this->table = convert.table;
        this->column = convert.column;
        this->row = convert.row;
    }

    SquareMatrix(int row) {
        this->row = row;
        this->column = row;
        this->table.resize(this->row, vector<double>(this->column, 0.0));
    }

    SquareMatrix operator+(SquareMatrix squarematrix2) {
        //squarematrix -> matrix
        Matrix *squarematrixptr2 = &squarematrix2;
        Matrix *squarematrixptr1 = this;
        Matrix final = (*squarematrixptr1) + (*squarematrixptr2);
        //matrix->squarematrix
        auto *ans = (SquareMatrix *) &final;
        return *ans;
    }

    SquareMatrix operator-(SquareMatrix squarematrix2) {
        Matrix *squarematrixptr2 = &squarematrix2;
        Matrix *squarematrixptr1 = this;
        Matrix final(squarematrix2.row, squarematrix2.column);
        final = (*squarematrixptr1) - (*squarematrixptr2);
        SquareMatrix *ans;
        ans = (SquareMatrix *) &final;
        return *ans;
    }

    SquareMatrix operator*(SquareMatrix squarematrix2) {
        Matrix *squarematrixptr2 = &squarematrix2;
        Matrix *squarematrixptr1 = this;
        Matrix final = (*squarematrixptr1) * (*squarematrixptr2);
        SquareMatrix *ans;
        ans = (SquareMatrix *) &final;
        return *ans;
    }

    ColumnVector operator*(ColumnVector columnvector1) {
        if (this->column == columnvector1.size) {
            ColumnVector res;
            res.size = this->row;
            res.data.resize(res.size);
            double sum;
            for (int i = 0; i < this->row; i++) {
                sum = 0;
                for (int j = 0; j < this->column; j++)
                    sum += this->table[i][j] * columnvector1.data[j];
                res.data[i] = sum;
            }
            return res;
        } else {
            cout << "Error: the dimensional problem occurred\n";
            ColumnVector res;
            return res;
        }
    }

    double calcDeterminant() {
        SquareMatrix a = *this;
        int n = a.column, k = 1;
        double det = 1;
        for (int i = 0; i < n - 1; i++) {
            //find max in column
            double ma = -1;
            int maindex = -1;
            for (int j = i; j < n; j++)
                if (abs(a.table[j][i]) > ma) {
                    maindex = j;
                    ma = abs(a.table[j][i]);
                }
            //change row if needed
            if (maindex != i) {
                det *= -1;
                swaprows(a, maindex, i);
            }
            maindex = i;
            //divide and subtract row
            for (int j = i + 1; j < n; j++)
                if (a.table[j][i] / a.table[i][i] != 0)
                    subtractRow(a, j, i);//row j-=coef*row i
        }
        for (int i = 0; i < n; i++)
            det *= a.table[i][i];
        return det;
    }

    bool diagonallyDominant() {
        for (int i = 0; i < row; i++) {
            double sum = 0;
            for (int j = 0; j < column; j++)
                if (i != j)
                    sum += abs(this->table[i][j]);
            if (fabs(this->table[i][i] < sum))
                return false;
        }
        return true;
    }

    SquareMatrix convertToDiagonal() {
        SquareMatrix res(row);
        for (int i = 0; i < row; i++)
            for (int j = 0; j < column; j++) {
                if (i != j)
                    res.table[i][j] = 0;
                else
                    res.table[i][j] = table[i][j];
            }
        return res;
    }

    //operator >>overload
    friend istream &operator>>(istream &is, SquareMatrix &matrix1) {
        is >> matrix1.row;
        matrix1.column = matrix1.row;
        matrix1.table.resize(matrix1.row, vector<double>(matrix1.column, 0.0));
        for (int i = 0; i < matrix1.row; i++)
            for (int j = 0; j < matrix1.column; j++)
                is >> matrix1.table[i][j];
        return is;
    }

    //operator << overload
    friend ostream &operator<<(ostream &os, SquareMatrix &matrix1) {
        for (int i = 0; i < matrix1.row; ++i) {
            for (int j = 0; j < matrix1.column; ++j) {
                if (matrix1.table[i][j] >= 0)
                    matrix1.table[i][j] = max(matrix1.table[i][j], 0.00);
                if (matrix1.table[i][j] <= 0 and fabs(matrix1.table[i][j]) <= 0.01)
                    matrix1.table[i][j] = 0.00;
                os << matrix1.table[i][j];
                if (j != matrix1.column - 1)
                    os << ' ';
            }
            os << '\n';
        }
        return os;
    }
};

class IdentityMatrix : public SquareMatrix {
public:
    IdentityMatrix() = default;

    IdentityMatrix(int row) {
        this->column = row;
        this->row = row;
        table.resize(row, vector<double>(column, 0.0));
        for (int i = 0; i < row; i++)
            table[i][i] = 1;
    }

    IdentityMatrix operator=(SquareMatrix m) {
        for (int i = 0; i < m.row; i++) {
            for (int j = 0; j < m.column; j++) {
                this->table[i][j] = m.table[i][j];
            }
        }
        return *this;
    }
};

class EliminationMatrix : public SquareMatrix {
    //identity matrix with -coefficients on 1,2 3,1 3,2
public:
    EliminationMatrix() = default;

    EliminationMatrix(const SquareMatrix &squarematrix1, int i, int j) {
        this->table = squarematrix1.table;
        this->row = i;
        this->column = j;
    }

    EliminationMatrix(int i, int j, Matrix &m) {
        i = i - 1;
        j = j - 1;
        IdentityMatrix im(m.row);
        double c = m.table[i][j] / m.table[j][j];
        for (int l = 0; l < m.row; l++)
            im.table[i][l] += -c * im.table[j][l];
        this->table = im.table;
        this->row = im.row;
        this->column = im.column;
    }

    EliminationMatrix geteliminationmatrix(Matrix a, int j, int i) {
        EliminationMatrix e;
        e.table = IdentityMatrix(a.column).table;
        int n = a.column;
        e.row = n;
        e.column = n;
        e.table.resize(a.row, vector<double>(a.column, 0.0));
        e.table[j][i] = -a.table[j][i] / a.table[i][i];
        return e;
    }
};

class PermutationMatrix : public SquareMatrix {
public:
    //swap i and j row - swap i j row in identity matrix
    PermutationMatrix() = default;

    PermutationMatrix(int i, int j, Matrix &m) {
        i = i - 1;
        j = j - 1;
        IdentityMatrix im(m.row);
        for (int l = 0; l < m.row; l++) {
            swap(im.table[i][l], im.table[j][l]);
        }
        this->table = im.table;
        this->row = im.row;
        this->column = im.column;
    }

    PermutationMatrix getpermutationmatrix(Matrix &a, int i, int j) {
        PermutationMatrix p;
        p.column = a.column;
        p.row = a.column;
        p.table = IdentityMatrix(p.column).table;
        swaprows(p, i - 1, j - 1);
        return p;
    }
};

double max(double x, double y) {
    if (x > y)
        return x;
    return y;
}

//class AugmentedMatrix : public Matrix{
//public:
//    //copy constructor
//    AugmentedMatrix(SquareMatrix a){
//        this->column=2*a.column;
//        this->row=a.row;
//        this->table.resize(row, vector<double>(column, 0.00));
//        for (int i=0;i<row;i++)
//            for (int j=0;j<column;j++)
//                this->table[i][j]=a.table[i][j];
//        for (int i=0;i<row;i++)
//            for (int j=a.column;j<column;j++)
//                this->table[i][j]=0;
//        for (int i=0;i<row;i++)
//            this->table[i][a.row+i]=1;
//    }
//};
class AugmentedMatrix : public Matrix {
public:
    SquareMatrix squareMatrix;
    IdentityMatrix identityMatrix;

    AugmentedMatrix(SquareMatrix squarematrix) {
        this->squareMatrix = squarematrix;
        this->row = squarematrix.row;
        this->column = squarematrix.column;
        this->identityMatrix = IdentityMatrix(squarematrix.column);
    }
};

AugmentedMatrix RREF(AugmentedMatrix augmentedmatrix) {
    int k = 1;
    for (int i = 0; i < augmentedmatrix.squareMatrix.column - 1; i++) {
        bool flag = false;
        double ma = fabs(augmentedmatrix.squareMatrix.table[i][i]);
        int index = i + 1;
        for (int j = i + 1; j < augmentedmatrix.squareMatrix.row; j++)
            if (fabs(augmentedmatrix.squareMatrix.table[j][i]) > ma) {
                ma = max(fabs(augmentedmatrix.squareMatrix.table[j][i]), ma);
                index = j;
                flag = true;
            }
        if (flag) {
            PermutationMatrix permutationMatrix(i + 1, index + 1, augmentedmatrix.squareMatrix);
            augmentedmatrix.squareMatrix = permutationMatrix * augmentedmatrix.squareMatrix;
            PermutationMatrix permutationMatrixIden(i + 1, index + 1, augmentedmatrix.identityMatrix);
            augmentedmatrix.identityMatrix = permutationMatrixIden * augmentedmatrix.identityMatrix;
            k += 1;
        }
        for (int j = i + 1; j < augmentedmatrix.squareMatrix.row; j++) {
            bool equal = true;
            EliminationMatrix eliminationMatrix(j + 1, i + 1, augmentedmatrix.squareMatrix);
            IdentityMatrix flagMatrix(eliminationMatrix.row);
            for (int h = 0; h < augmentedmatrix.squareMatrix.row; h++)
                for (int l = 0; l < augmentedmatrix.squareMatrix.column; l++)
                    if (eliminationMatrix.table[h][l] != flagMatrix.table[h][l]) {
                        equal = false;
                        break;
                    }
            if (!equal) {
                augmentedmatrix.squareMatrix = eliminationMatrix * augmentedmatrix.squareMatrix;
                augmentedmatrix.identityMatrix = eliminationMatrix * augmentedmatrix.identityMatrix;
                k += 1;
            }
        }
    }
// Way back
    for (int i = augmentedmatrix.squareMatrix.column - 1; i > 0; i--)
        for (int j = i - 1; j >= 0; j--) {
            EliminationMatrix eluminationMatrix(j + 1, i + 1, augmentedmatrix.squareMatrix);
            bool equal = true;
            IdentityMatrix flag(eluminationMatrix.row);
            for (int h = 0; h < augmentedmatrix.squareMatrix.row; h++)
                for (int l = 0; l < augmentedmatrix.squareMatrix.column; l++)
                    if (eluminationMatrix.table[h][l] != flag.table[h][l]) {
                        equal = false;
                        break;
                    }
            if (!equal) {
                augmentedmatrix.squareMatrix = eluminationMatrix * augmentedmatrix.squareMatrix;
                augmentedmatrix.identityMatrix = eluminationMatrix * augmentedmatrix.identityMatrix;
                k += 1;
            }
        }
    return augmentedmatrix;
}

AugmentedMatrix normalize(AugmentedMatrix &matrix) {
    for (int i = 0; i < matrix.squareMatrix.row; i++) {
        double m = matrix.squareMatrix.table[i][i];
        for (int j = 0; j < matrix.squareMatrix.column; j++)
            if (i == j)
                for (int k = 0; k < matrix.row; k++) {
                    matrix.squareMatrix.table[i][k] = matrix.squareMatrix.table[i][k] / m;
                    matrix.identityMatrix.table[i][k] = matrix.identityMatrix.table[i][k] / m;
                    if (matrix.identityMatrix.table[i][k] >= 0)
                        matrix.identityMatrix.table[i][k] = max(matrix.identityMatrix.table[i][k], 0.00);
                    if (matrix.identityMatrix.table[i][k] <= 0 and fabs(matrix.identityMatrix.table[i][k]) <= 0.0001)
                        matrix.identityMatrix.table[i][k] = 0.00;
                }
    }
    return matrix;
}

Matrix inverse(Matrix matrix1) {
    if (matrix1.row != matrix1.column) {
        cout << "Error: the dimensional problem occurred\n";
        return Matrix();
    }
    AugmentedMatrix augmentedInvertMatrix = AugmentedMatrix(matrix1);
    int n = augmentedInvertMatrix.row;
    for (int i = 0; i < n - 1; i++) {
        //find max in column
        double ma = -1;
        int maindex = -1;
        for (int j = i; j < n; j++)
            if (abs(augmentedInvertMatrix.table[j][i]) > ma) {
                maindex = j;
                ma = abs(augmentedInvertMatrix.table[j][i]);
            }
        //change row if needed
        if (maindex != i)
            swaprows(augmentedInvertMatrix, maindex, i);
        //divide and subtract row
        for (int j = i + 1; j < n; j++)
            if (augmentedInvertMatrix.table[j][i] / augmentedInvertMatrix.table[i][i] != 0)
                subtractRow(augmentedInvertMatrix, j, i);//row j-=coef*row i
    }
    for (int i = augmentedInvertMatrix.row - 1; i > 0; i--)
        for (int j = i - 1; j >= 0; j--)
            subtractRow(augmentedInvertMatrix, j, i);

    double coef;
    for (int i = 0; i < augmentedInvertMatrix.row; i++) {
        coef = augmentedInvertMatrix.table[i][i];
        for (int j = i; j < augmentedInvertMatrix.column; j++)
            augmentedInvertMatrix.table[i][j] /= coef;
    }
    SquareMatrix invertMatrix = SquareMatrix();
    invertMatrix.row = augmentedInvertMatrix.row;
    invertMatrix.column = augmentedInvertMatrix.row;
    invertMatrix.table.resize(invertMatrix.row, vector<double>(invertMatrix.column, 1.0));
    for (int i = 0; i < invertMatrix.row; i++)
        for (int j = 0; j < invertMatrix.row; j++)
            invertMatrix.table[i][j] = augmentedInvertMatrix.table[i][invertMatrix.row + j];
    return invertMatrix;
}

ColumnVector solveequation(SquareMatrix &a, ColumnVector &b) {
    int n = a.row;
    for (int i = 0; i < n - 1; i++) {
        //find max in column
        double ma = -1;
        int maindex = -1;
        for (int j = i; j < n; j++)
            if (abs(a.table[j][i]) > ma) {
                maindex = j;
                ma = abs(a.table[j][i]);
            }
        //change row if needed
        if (maindex != i) {
            swaprows(a, maindex, i);
            swap(b.data[maindex], b.data[i]);
        }
        //divide and subtract row
        for (int j = i + 1; j < n; j++)
            if (a.table[j][i] / a.table[i][i] != 0) {
                b.data[j] -= b.data[i] * (a.table[j][i] / a.table[i][i]);
                subtractRow(a, j, i);
            }
    }
    //Way back
    for (int i = a.row - 1; i > 0; i--)
        for (int j = i - 1; j >= 0; j--) {
            b.data[j] -= ((a.table[j][i] / a.table[i][i]) * b.data[i]);
            subtractRow(a, j, i);
        }
    for (int i = 0; i < a.row; i++) {
        b.data[i] /= a.table[i][i];
        a.table[i][i] = 1;
    }
    return b;
}

class LST2Dimension {
public:////
    vector<vector<double>> xySet;
    Matrix matrixx;
    ColumnVector b;
    ColumnVector x;
    int polynomialDegree;
public:
    //operator >>overload
    friend istream &operator>>(istream &is, LST2Dimension &LST2Dimension1) {
        is >> LST2Dimension1.matrixx.row;
        LST2Dimension1.xySet.resize(LST2Dimension1.matrixx.row, vector<double>(2, 1.0));
        for (int i = 0; i < LST2Dimension1.matrixx.row; i++)
            for (int j = 0; j < 2; j++)
                is >> LST2Dimension1.xySet[i][j];
        is >> LST2Dimension1.polynomialDegree;
        LST2Dimension1.matrixx.column = LST2Dimension1.polynomialDegree + 1;
        LST2Dimension1.matrixx.table.resize(LST2Dimension1.matrixx.row,
                                            vector<double>(LST2Dimension1.matrixx.column, 1.0));
        LST2Dimension1.b.data.resize(LST2Dimension1.matrixx.row);
        LST2Dimension1.b.size = LST2Dimension1.matrixx.row;
        for (int i = 0; i < LST2Dimension1.matrixx.row; i++) {
            for (int j = 0; j < LST2Dimension1.matrixx.column; j++)
                LST2Dimension1.matrixx.table[i][j] = pow(LST2Dimension1.xySet[i][0], j);
            LST2Dimension1.b.data[i] = LST2Dimension1.xySet[i][1];
        }
        return is;
    }

    ColumnVector findLinearApproximation() {
        ColumnVector ans = inverse((this->matrixx.transpose() * this->matrixx)) * matrixx.transpose() * b;
        return ans;
    }
};
void solveJacobian(SquareMatrix A, ColumnVector b, double approximAcc){
    SquareMatrix D, D_1, a;
    if (!A.diagonallyDominant()) {
        cout << "The method is not applicable!\n";
        exit(0);
    }
    D = A.convertToDiagonal();
    AugmentedMatrix augmentedMatrix(D);
    AugmentedMatrix rref = RREF(augmentedMatrix);
    augmentedMatrix = normalize(rref);
    D_1 = augmentedMatrix.identityMatrix;
    IdentityMatrix I(b.size);
    a = I - (D_1 * A);
    ColumnVector beta = D_1 * b;
    cout << fixed << showpoint << setprecision(4);
    cout << "alpha:\n" << a << "beta:\n" << beta;
    int counter = 0;
    double epsilon = 0;
    ColumnVector x0 = beta;
    ColumnVector x1(b.size);
    while (true) {
        cout << "x(" << counter << "):\n";
        counter++;
        ColumnVector temp1 = (A * x0);
        ColumnVector temp2 = (b - temp1);
        ColumnVector temp3 = (D_1 * temp2);
        x1 = x0 + temp3;
        epsilon = 0;
        for (int i = 0; i < b.size; i++)
            epsilon += pow(temp2.data[i] / A.table[i][i], 2);
        cout << x0 << "e: " << sqrt(epsilon) << "\n";
        x0 = x1;
        if (sqrt(epsilon) < approximAcc)
            break;
    }
    cout << "x(" << counter << "):\n" << x0;
}
class PredatorPreyData{
public:
    vector <double> nVictimsAtTime;
    vector <double> nKillersAtTime;
    vector <double> time;
    PredatorPreyData(){
        nVictimsAtTime.resize(0);
        nKillersAtTime.resize(0);
        time.resize(0);
    }
};
double numVictimAtTime(double time,double initNumVictims, double initNumKillers, double nApproximationPoints, double a1, double b1, double a2,double b2){
    return (initNumVictims-a2/b2)*cos(::sqrt(a1*a2)*time)-(initNumKillers-a1/b1)*(sqrt(a2)*b1/(b2* sqrt(a1)))*sin(sqrt(a1*a2)*time)+a2/b2;
}
double numKillerAtTime(double time,double initNumVictims, double initNumKillers, double nApproximationPoints, double a1, double b1, double a2,double b2){
    return (initNumVictims-a2/b2)*(sqrt(a1)*b2)/(b1* sqrt(a2))*sin(sqrt(a1*a2)*time)+(initNumKillers-a1/b1)*cos(sqrt(a1*a2)*time)+a1/b1;
}
PredatorPreyData calcPredatorPreyModel(double initNumVictims, double initNumKillers,double timeLimit, double nApproximationPoints, double a1, double b1, double a2,double b2){
    PredatorPreyData predatorpreydata;
    for (double time=0;time<=timeLimit;time+=timeLimit/nApproximationPoints){
        predatorpreydata.nVictimsAtTime.push_back(numVictimAtTime(time,initNumVictims, initNumKillers, nApproximationPoints, a1, b1, a2, b2));
        predatorpreydata.nKillersAtTime.push_back(numKillerAtTime(time,initNumVictims, initNumKillers, nApproximationPoints, a1, b1, a2, b2));
        predatorpreydata.time.push_back(time);
    }
    return predatorpreydata;
}
int main() {
////Example of simple Matrix operations
//    LST2Dimension dataSet;
//    cin>>dataSet;
//    Matrix ATA=dataSet.matrixx.transpose()*dataSet.matrixx;
//    Matrix ATA1= inverse(dataSet.matrixx.transpose()*dataSet.matrixx);
//    ColumnVector ATb=dataSet.matrixx.transpose()*dataSet.b;
//    dataSet.matrixx*dataSet.matrixx.transpose();
//    ColumnVector x=dataSet.findLinearApproximation();
//    cout << fixed << showpoint << setprecision(4);
//    cout<<"A:\n"<<dataSet.matrixx<<"A_T*A:\n"<<ATA<<"(A_T*A)^-1:\n"<<ATA1<<"A_T*b:\n"<<ATb<<"x~:\n"<<x;

////Jacobian method to solve Ax=b
//    SquareMatrix A;
//    ColumnVector b;
//    double approximAcc;
//    cin >> A >> b >> approximAcc;
//    solveJacobian(A,b,approximAcc);

////Predator-Prey model
    double initNumVictims, initNumKillers,timeLimit,nApproximationPoints, a1,b1,a2,b2;
    cin>>initNumVictims>>initNumKillers>>a1>>b1>>a2>>b2>>timeLimit>>nApproximationPoints;
    PredatorPreyData predatorpreydata =calcPredatorPreyModel(initNumVictims, initNumKillers,timeLimit,nApproximationPoints,a1,b1,a2,b2);
    cout << fixed << showpoint << setprecision(2);
    cout<<"t:\n";
    for (int i=0;i<predatorpreydata.time.size();i++)
        cout<<predatorpreydata.time[i]<<" ";
    cout<<"\nv:\n";
    for (int i=0;i<predatorpreydata.time.size();i++)
        cout<<predatorpreydata.nVictimsAtTime[i]<<" ";
    cout<<"\nk:\n";
    for (int i=0;i<predatorpreydata.time.size();i++)
        cout<<predatorpreydata.nKillersAtTime[i]<<" ";
    return 0;
}
