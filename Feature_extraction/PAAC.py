import re, sys, os, platform
import math
pPath = os.path.split(os.path.realpath('__file__'))[0]
sys.path.append(pPath)
import checkFasta
import readFasta
import saveCode
import numpy as np
import pandas as pd

USAGE = """
USAGE:
	python PAAC.py input.fasta <lambda> <output>

	input.fasta:      the input protein sequence file in fasta format.
	lambda:           the lambda value, integer, defaule: 30
	output:           the encoding file, default: 'encodings.tsv'
"""

def Rvalue(aa1, aa2, AADict, Matrix):
	return sum([(Matrix[i][AADict[aa1]] - Matrix[i][AADict[aa2]]) ** 2 for i in range(len(Matrix))]) / len(Matrix)

def PAAC(fastas, lambdaValue=2, w=0.05, **kw):#w is the weighting factor
	if checkFasta.minSequenceLengthWithNormalAA(fastas) < lambdaValue + 1:
		print('Error: all the sequence length should be larger than the lambdaValue+1: ' + str(lambdaValue + 1) + '\n\n')
		return 0

	dataFile = re.sub('codes$', '', os.path.split(os.path.realpath('__file__'))[0]) + r'\PAAC.txt' if platform.system() == 'Windows' else re.sub('codes$', '', os.path.split(os.path.realpath(__file__))[0]) + '/data/PAAC.txt'
	with open(dataFile) as f:#Hydrophobicity, hydrophilicity, side chain quality
		records = f.readlines()
	AA = ''.join(records[0].rstrip().split()[1:])
	AADict = {}
	for i in range(len(AA)):#len(AA)=20
		AADict[AA[i]] = i
	AAProperty = []
	AAPropertyNames = []
	for i in range(1, len(records)):#len(records)=4
		array = records[i].rstrip().split() if records[i].rstrip() != '' else None
		AAProperty.append([float(j) for j in array[1:]])#Represent the 20 common amino acid values corresponding to the 3 amino acid indicators
		AAPropertyNames.append(array[0])#Hydrophobicity, hydrophilicity, side chain quality

	AAProperty1 = []
	for i in AAProperty:
		meanI = sum(i) / 20
		fenmu = math.sqrt(sum([(j-meanI)**2 for j in i])/20)
		AAProperty1.append([(j-meanI)/fenmu for j in i])

	encodings = []
	header = ['#']
	for aa in AA:
		header.append('Xc1.' + aa)
	for n in range(1, lambdaValue + 1):
		header.append('Xc2.lambda' + str(n))
	encodings.append(header)

	for i in fastas:
		name, sequence = i[0], re.sub('-', '', i[1])
		code = [name]
		theta = []
		for n in range(1, lambdaValue + 1):
			theta.append(
				sum([Rvalue(sequence[j], sequence[j + n], AADict, AAProperty1) for j in range(len(sequence) - n)]) / (
				len(sequence) - n))
		myDict = {}
		for aa in AA:
			myDict[aa] = sequence.count(aa)
		code = code + [myDict[aa] / (1 + w * sum(theta)) for aa in AA]
		code = code + [(w * j) / (1 + w * sum(theta)) for j in theta]
		encodings.append(code)
	return encodings #Calculate pseudo amino acid composition vector


    
fastas = readFasta.readFasta(r"data_AMP.txt")
result=PAAC(fastas, lambdaValue=2, w=0.05)
data=np.matrix(result[1:])
data_PseAAC=pd.DataFrame(data=data[:,1:])
sio.savemat('pseaac_AMP.mat', {'pseaac_AMP':data_PseAAC})