import PSSMmaker

#PSSMMaker.command_pssm('E:\Blast\db\Cytoplasm_test.fasta','try_once.txt','pssm1.pssm')
proseq=r'raw_data\data_AMP.txt'
outdir=r'pssm_data'
PSSMmaker.pssm(proseq,outdir)

